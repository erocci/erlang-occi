%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_text).

-include("occi.hrl").
-include("occi_log.hrl").
-include_lib("annotations/include/annotations.hrl").

-export([parse_model/2,
	 parse_entity/3,
	 parse_collection/2,
	 parse_invoke/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec parse_model(extension | kind | mixin | action, binary()) -> occi_type:t().
parse_model(mixin, Bin) ->
    parse_mixin(occi_parser_http:parse(Bin));

parse_model(Type, _Bin) when Type =:= extension;
			     Type =:= kind;
			     Type =:= mixin;
			     Type =:= actin ->
    throw({not_implemented, Type}).


-spec parse_entity(entity | resource | link, binary(), occi_ctx:t()) -> occi_type:t().
parse_entity(Type, Bin, Ctx) when Type =:= entity;
				  Type =:= resource;
				  Type =:= link ->
    Entity = parse_entity2(occi_parser_http:parse(Bin), Ctx),
    case occi_entity:is_subtype(Type, Entity) of
	true -> Entity;
	false -> throw({parse_error, {type, Entity}})
    end.


-spec parse_collection(iolist(), occi_ctx:t()) -> occi_collection:t().
parse_collection(Bin, Ctx) ->
    Headers = occi_parser_http:parse(Bin),
    Locations = p_locations(orddict:find('x-occi-location', Headers), Ctx),
    C = occi_collection:new(),
    occi_collection:append(Locations, C).


-spec parse_invoke(iolist()) -> occi_invoke:t().
parse_invoke(Bin) ->
    Headers = occi_parser_http:parse(Bin),
    p_invoke(Headers).

%%%
%%% Parsers
%%%
p_invoke(Headers) ->
    case p_categories(orddict:find('category', Headers)) of
	[] ->
	    throw({parse_error, {action, no_category}});
	[{category, #{ class := action, scheme := Scheme, term := Term }}] ->
	    p_invoke2({Scheme, Term}, p_attributes(orddict:find('x-occi-attribute', Headers)));
	_ ->
	    throw({parse_error, {action, multiple_categories}})
    end.


p_invoke2(Id, Attributes) ->
    Map = lists:foldl(fun ({attribute, Key, {_, Value}}, Acc) ->
			      Acc#{ Key => Value }
		      end, #{}, Attributes),
    occi_invoke:new(Id, Map).


parse_mixin(H) ->
    case p_categories(orddict:find('category', H)) of
	[] ->
	    throw({parse_error, {mixin, no_mixin}});
	[{category, #{ class := mixin, scheme := Scheme, term := Term }=Map }] ->
	    M0 = occi_mixin:new(Scheme, Term),
	    Location = maps:get(location, Map),
	    M1 = occi_mixin:location(Location, M0),
	    case maps:get(title, Map, undefined) of
		undefined ->
		    M1;
		Title ->
		    occi_mixin:title(Title, M1)
	    end;    
	_ ->
	    throw({parse_error, {mixin, multiple_mixin}})
    end.


parse_entity2(H, Ctx) ->
    Filter = fun ({category, #{ class := kind, scheme := Scheme, term := Term }},
		  {undefined, Mixins}) ->
		     { {Scheme, Term}, Mixins };
		 ({category, #{ class := kind}}, _Acc) ->
		     throw({parse_error, {entity, kind_already_defined}});
		 ({category, #{ class := mixin, scheme := Scheme, term := Term }},
		  {Kind, Mixins}) ->
		     { Kind, [ {Scheme, Term} | Mixins ]};
		 ({category, # { class := action }}, Acc) ->
		     %% ignore ??
		     Acc
	     end,
    {KindId, MixinIds} = lists:foldl(Filter, {undefined, []}, p_categories(orddict:find('category', H))),
    Attributes = p_attributes(orddict:find('x-occi-attribute', H)),
    {Id, Attributes2} = case lists:keytake(<<"occi.core.id">>, 2, Attributes) of
			    {value, {attribute, _, {string, SId}}, Rest} ->
				{occi_uri:from_string(SId, Ctx), Rest};
			    {value, {attribute, _, {Type, _}}, _} ->
				throw({parse_error, {entity, {<<"occi.core.id">>, Type}}});
			    false ->
				throw({parse_error, {entity, {<<"occi.core.id">>, <<>>}}})
			end,
    Kind = occi_models:category(KindId),
    case occi_kind:has_parent(resource, Kind) of
	true ->
	    Links = p_links(orddict:find('link', H)),
	    p_resource(Id, Kind, MixinIds, Attributes2, Links, Ctx);
	false ->
	    p_link(Id, Kind, MixinIds, Attributes2, Ctx)
    end.


p_resource(Id, Kind, MixinIds, Attributes, Links, Ctx) ->
    R = occi_resource:new(Id, Kind),
    R1 = lists:foldl(fun (Mixin, Acc) ->
			     occi_entity:add_mixin(Mixin, Acc)
		     end, R, MixinIds),
    R2 = lists:foldl(fun ({link, Link}, Acc) ->
			     occi_resource:add_link(p_resource_link(Id, Kind, Link, Ctx), Acc)
		     end, R1, Links),
    occi_entity:set(lists:foldl(fun ({attribute, Key, {_, Value}}, Acc) ->
					Acc#{ Key => Value }
				end, #{}, Attributes), Ctx, R2).


p_resource_link(Source, SourceKind, Link, Ctx) ->
    Id = case maps:get(self, Link, undefined) of
	     undefined ->
		 occi_utils:urn(Source);
	     Self ->
		 occi_uri:from_string(Self, Ctx)
	 end,
    Categories = maps:get(categories, Link, [?link_kind_id]),
    {Kind, MixinIds} = filter_categories(Categories, undefined, []),
    Target = occi_uri:from_string(maps:get(target, Link), Ctx),
    [ TargetKind | _ ] = maps:get(rel, Link),
    Attributes = maps:get(attributes, Link),
    p_link2(Id, Kind, MixinIds, Source, occi_kind:id(SourceKind), 
	    Target, TargetKind, Attributes, Ctx).


p_link(Id, Kind, MixinIds, Attributes, Ctx) ->
    {Source, Attrs2} = case lists:keytake(<<"occi.core.source">>, 2, Attributes) of
			   {value, {attribute, _, {string, V}}, Rest} ->
			       {occi_uri:from_string(V, Ctx), Rest};
			   {value, {attribute, _, {Type, _}}, _} ->
			       throw({parse_error, {link, {<<"occi.core.source">>, Type}}});
			   false ->
			       throw({parse_error, {link, {<<"occi.core.source">>, <<>>}}})
		       end,
    {SourceKind, Attrs3} = case lists:keytake(<<"occi.core.source.kind">>, 2, Attrs2) of
			       {value, {attribute, _, {string, V1}}, Rest1} ->
				   {V1, Rest1};
			       {value, {attribute, _, {Type1, _}}, _} ->
				   throw({parse_error, {link, {<<"occi.core.source.kind">>, Type1}}});
			       false ->
				   {undefined, Attrs2}
			   end,
    {Target, Attrs4} = case lists:keytake(<<"occi.core.target">>, 2, Attrs3) of
			   {value, {attribute, _, {string, V2}}, Rest2} ->
			       {occi_uri:from_string(V2, Ctx), Rest2};
			   {value, {attribute, _, {Type2, _}}, _} ->
			       throw({parse_error, {link, {<<"occi.core.target">>, Type2}}});
			   false ->
			       throw({parse_error, {link, {<<"occi.core.target">>, <<>>}}})
		       end,
    {TargetKind, Attrs5} = case lists:keytake(<<"occi.core.target.kind">>, 2, Attrs4) of
			       {value, {attribute, _, {string, V3}}, Rest3} ->
				   {V3, Rest3};
			       {value, {attribute, _, {Type3, _}}, _} ->
				   throw({parse_error, {link, {<<"occi.core.target.kind">>, Type3}}});
			       false ->
				   {undefined, Attrs4}
			   end,
    p_link2(Id, Kind, MixinIds, Source, SourceKind, Target, TargetKind, Attrs5, Ctx).


p_link2(Id, Kind, MixinIds, Source, SourceKind, Target, TargetKind, Attributes, Ctx) ->
    L = occi_link:new(Id, Kind, Source, SourceKind, Target, TargetKind),
    L1 = lists:foldl(fun (MixinId, Acc) ->
			     occi_link:add_mixin(MixinId, Acc)
		     end, L, MixinIds),
    occi_link:set(lists:foldl(fun ({attribute, Key, {_, Value}}, Acc) ->
				      Acc#{ Key => Value }
			      end, #{}, Attributes), Ctx, L1).
    

%%%
%%% Parse functions
%%%
-define(is_alpha(C), C >= 65, C =< 90; C >= 97, C =< 122).
-define(is_digit(C), C >= 48, C =< 57).

p_locations(error, _Ctx) ->
    [];

p_locations({ok, Values}, Ctx) ->
    p_locations(Values, [], Ctx).


p_locations([], Acc, _Ctx) ->
    lists:reverse(Acc);

p_locations([ Bin | Tail ], Acc, Ctx) ->
    case p_location_value(eat_ws(Bin)) of
	{location, Location} ->
	    p_locations(Tail, [ occi_uri:from_string(Location, Ctx) | Acc ], Ctx);
	Else ->
	    throw({parse_error, {invalid_location, Else}})
    end.


p_categories(error) -> 
    [];

p_categories({ok, Values}) -> 
    p_categories(Values, []).


p_categories([], Acc) ->
    lists:reverse(Acc);

p_categories([ Bin | Tail ], Acc) -> 
    p_categories(Tail, [ p_category(p_kv(Bin)) | Acc ]).


p_category(undefined) ->
    throw({parse_error, {category, missing_term}});

p_category({term, Term, Rest}) when is_binary(Term) ->
    p_category_scheme(p_kv(Rest), #{ term => Term }).


p_category_scheme({scheme, Scheme, Rest}, Cat) ->
    p_category_class(p_kv(Rest), Cat#{ scheme => Scheme });

p_category_scheme({Type, Else, _Rest}, _Cat) ->
    throw({parse_error, {scheme, {Type, Else}}}).


p_category_class({class, Class, Rest}, Cat) ->
    p_category2(p_kv(Rest), Cat#{ class => Class });

p_category_class({Type, Value, _Rest}, _Cat) ->
    throw({parse_error, {class, {Type, Value}}}).


p_category2(undefined, Cat) ->
    {category, Cat};

p_category2({title, Title, Rest}, Cat) ->
    p_category2(p_kv(Rest), Cat#{ title => Title });

p_category2({rel, Rel, Rest}, Cat) when is_binary(Rel) ->
    p_category2(p_kv(Rest), Cat#{ rel => Rel });

p_category2({location, Location, Rest}, Cat) ->
    p_category2(p_kv(Rest), Cat#{ location => Location });

p_category2({attributes, Attributes, Rest}, Cat) ->
    p_category2(p_kv(Rest), Cat#{ attributes => Attributes });

p_category2({actions, Actions, Rest}, Cat) ->
    p_category2(Rest, Cat#{ actions => Actions });

p_category2({Type, Value, _Rest}, __Cat) ->
    throw({parse_error, {category, {Type, Value}}});

p_category2(Else, _Cat) ->
    throw({parse_error, {category, Else}}).


p_attributes(error) ->
    [];

p_attributes({ok, Values}) ->
    p_attributes(Values, []).


p_attributes([], Acc) ->
    lists:reverse(Acc);

p_attributes([ Bin | Tail ], Acc) ->
    case p_attribute(p_kv(Bin)) of
	undefined ->
	    p_attributes(Tail, Acc);
	{attribute, _, _}=Attr ->
	    p_attributes(Tail, [ Attr | Acc ])
    end.


p_attribute(undefined) ->
    undefined;

p_attribute({kv, {Key, {Type, Value}}, <<>>}) ->
    {attribute, Key, {Type, Value}};

p_attribute({kv, _, Rest}) ->
    throw({parse_error, {attribute, Rest}}).


p_links(error) ->
    [];

p_links({ok, Values}) ->
    p_links(Values, []).


p_links([], Acc) ->
    lists:reverse(Acc);

p_links([ Bin | Tail ], Acc) ->
    p_links(Tail, [ p_link(Bin) | Acc ]).


p_link(<<>>) ->
    throw({parse_error, {link, <<>>}});

p_link(Bin) ->
    case p_uri_ref(Bin) of
	{'uri-ref', Target, << $;, Rest/binary >>} ->
	    p_link_rel(p_kv(eat_ws(Rest)), #{ target => Target, attributes => [] });
	{'uri-ref', _Target, <<>>} ->
	    throw({parse_error, {link, <<>>}});
	{'uri-ref', _Target, << C, _Rest/binary >>} ->
	    throw({parse_error, {link, C}})
    end.


p_link_rel({rel, Rel, Rest}, Link) when is_list(Rel) ->
    p_link_self(p_kv(Rest), Link#{ rel => Rel });

p_link_rel({rel, Rel, Rest}, Link) when is_binary(Rel) ->
    p_link_self(p_kv(Rest), Link#{ rel => [Rel] });

p_link_rel({Type, Value, _Rest}, _Link) ->
    throw({parse_error, {rel, {Type, Value}}}).


p_link_self(undefined, Link) ->
    {link, Link};

p_link_self({self, Self, Rest}, Link) ->
    p_link_category(p_kv(Rest), Link#{ self => Self });

p_link_self({category, _, _}=Cat, Link) ->
    p_link_category(Cat, Link);

p_link_self({Type, Value, _Rest}, _Link) ->
    throw({parse_error, {self, {Type, Value}}}).


p_link_category(undefined, Link) ->
    {link, Link};

p_link_category({category, Categories, Rest}, Link) ->
    p_link_attribute(p_kv(Rest), Link#{ categories => Categories });

p_link_category({kv, _, _}=Attr, Link) ->
    p_link_attribute(Attr, Link);

p_link_category({Type, Value, _Rest}, _Link) ->
    throw({parse_error, {Type, Value}}).


p_link_attribute(undefined, #{ attributes := Attributes }=Link) ->
    {link, Link#{ attributes := lists:reverse(Attributes) }};

p_link_attribute({kv, {Key, Value}, Rest}, #{ attributes := Attributes }=Link) ->
    p_link_attribute(p_kv(Rest), Link#{ attributes := [ {Key, Value} | Attributes ]});

p_link_attribute({Type, Value, _Rest}, _Link) ->
    throw({parse_error, {link, {Type, Value}}}).


p_kv(<<>>) ->
    undefined;

p_kv(Bin) ->
    p_key(Bin).


p_key(<< "actions=", Rest/bits >>) ->
    p_value(Rest, actions);

p_key(<< "attributes=", Rest/bits >>) ->
    p_value(Rest, attributes);

p_key(<< "category=", Rest/bits >>) ->
    p_value(Rest, category);

p_key(<< "class=", Rest/bits >>) ->
    p_value(Rest, class);

p_key(<< "location=", Rest/bits >>) ->
    p_value(Rest, location);

p_key(<< "rel=", Rest/bits >>) ->
    p_value(Rest, rel);

p_key(<< "self=", Rest/bits >>) ->
    p_value(Rest, self);

p_key(<< "scheme=", Rest/bits >>) ->
    p_value(Rest, scheme);

p_key(<< "title=", Rest/bits >>) ->
    p_value(Rest, title);

p_key(Bin) ->
    p_key2(Bin, <<>>).


p_key2(<< C, Rest/binary >>, Acc) when ?is_alpha(C);
				       ?is_digit(C) ->
    p_key3(Rest, << Acc/binary, C >>);

p_key2(<< C, _Rest/binary >>, _Acc) ->
    throw({parse_error, {key, C}}).


p_key3(<<>>, Acc) ->
    {term, Acc, <<>>};

p_key3(<< $=, Rest/binary >>, Acc) ->
    p_value(Rest, Acc);

p_key3(<< $;, Rest/binary >>, Acc) ->
    {term, Acc, eat_ws(Rest)};

p_key3(<< C, Rest/binary >>, Acc) when ?is_alpha(C);
				       ?is_digit(C);
				       C =:= $-;
				       C =:= $_;
				       C =:= $. ->
    p_key3(Rest, << Acc/binary, C >>);

p_key3(<< C, _Rest/binary >>, _Acc) ->
    throw({parse_error, {key, C}}).


p_value(Bin, attributes) ->
    p_attributes_def(Bin);

p_value(Bin, actions) ->
    {rel, Actions, Rest} = p_rel(Bin),
    {actions, Actions, Rest};

p_value(Bin, category) ->
    p_link_type(eat_ws(Bin), <<>>, []);

p_value(Bin, class) ->
    p_class(Bin);

p_value(Bin, location) ->
    p_location(Bin);

p_value(Bin, rel) ->
    p_rel(Bin);

p_value(Bin, self) ->
    {qs, Self, Rest} = p_qs(Bin, $", $"),
    {self, Self, Rest};

p_value(Bin, scheme) ->
    p_scheme(Bin);

p_value(Bin, title) ->
    {qs, S, Rest} = p_qs(Bin, $", $"),
    {title, S, Rest};

p_value(Bin, Key) ->
    p_value2(Bin, Key).


p_value2(<< "true" >>, Key) ->
    {kv, {Key, true}, <<>>};

p_value2(<< "true", C, Rest/binary >>, Key) when C =:= $\s;
						 C =:= $, ->
    {kv, {Key, true}, eat_ws(Rest)};

p_value2(<< "false" >>, Key) ->
    {kv, {Key, false}, <<>>};

p_value2(<< "false", C, Rest/binary >>, Key) when C =:= $\s;
						  C =:= $, ->
    {kv, {Key, false}, eat_ws(Rest)};

p_value2(<< $", Rest/binary >>, Key) ->
    {string, Value, Rest2} = p_string(Rest, <<>>),
    {kv, {Key, {string, Value}}, Rest2};

p_value2(<< C, Rest/binary >>, Key) when ?is_digit(C);
					 C =:= $+;
					 C =:= $- ->
    {Type, Value, Rest2} = p_number(Rest, << C >>),
    {kv, {Key, {Type, Value}}, Rest2};

p_value2(<< $., Rest/binary >>, Key) ->
    {float, Value, Rest2} = p_float(Rest, << $. >>),
    {kv, {Key, {float, Value}}, Rest2};

p_value2(<< C, Rest/binary >>, Key) when ?is_alpha(C) ->
    End = fun (X) when X =:= $\s;
		       X =:= $;;
		       X =:= $, ->
		  true;
	      (_) ->
		  false
	  end,
    {alnum, Value, Rest2} = p_alnum(Rest, End, << C >>),
    {kv, {Key, {alnum, Value}}, Rest2};

p_value2(<< C, _Rest/binary >>, _Key) ->
    throw({parse_error, {value, C}}).


p_location_value(<<>>) ->
    throw({parse_error, {location, <<>>}});

p_location_value(<< C, Rest/binary >>) ->
    p_location_value2(Rest, <<C>>).


p_location_value2(<<>>, Acc) ->
    {location, Acc};

p_location_value2(<< $\n, Rest/binary >>, Acc) ->
    p_location_value3(eat_ws(Rest), Acc);

p_location_value2(<< C, Rest/binary >>, Acc) ->
    p_location_value2(Rest, << Acc/binary, C >>).


p_location_value3(<<>>, Acc) ->
    {location, Acc};

p_location_value3(<< C, _Rest/binary >>, _Acc) ->
    throw({parse_error, {location, C}}).


p_location(<<>>) ->
    throw({parse_error, {location, <<>>}});

p_location(<< $", Rest/binary >>) ->
    p_location2(Rest, <<>>);

p_location(<< C, _Rest/binary >>) ->
    throw({parse_error, {location, C}}).


p_location2(<<>>, _Acc) ->
    throw({parse_error, {location, <<>>}});

p_location2(<< C, _Rest/binary >>, _Acc) when C =:= $\n;
					      C =:= $\s;
					      C =:= $\t ->
    throw({parse_error, {location, C}});

p_location2(<< $", Rest/binary >>, Acc) ->
    p_location3(eat_ws(Rest), Acc);

p_location2(<< C, Rest/binary >>, Acc) ->
    p_location2(Rest, << Acc/binary, C >>).


p_location3(<<>>, Acc) ->
    {location, Acc, <<>>};

p_location3(<< $;, Rest/binary >>, Acc) ->
    {location, Acc, Rest};

p_location3(<< C, _Rest/binary >>, _Acc) ->
    throw({parse_error, {location, C}}).
    

p_rel(<<>>) ->
    throw({parse_error, {rel, <<>>}});

p_rel(<< $", Rest/binary >>) ->
    p_rel2(Rest, <<>>);

p_rel(<< C, _Rest/binary >>) ->
    throw({parse_error, {rel, C}}).


p_rel2(<<>>, _Acc) ->
    throw({parse_error, {rel, <<>>}});

p_rel2(<< $", Rest/binary >>, Acc) ->
    p_rel4(eat_ws(Rest), Acc);

p_rel2(<< $\s, Rest/binary >>, Acc) ->
    p_rel3(eat_ws(Rest), <<>>, [Acc]);

p_rel2(<< C, Rest/binary >>, Acc) ->
    p_rel2(Rest, << Acc/binary, C >>).


p_rel3(<<>>, _, _) ->
    throw({parse_error, {rel, <<>>}});

p_rel3(<< $", Rest/binary >>, <<>>, Rels) ->
    p_rel4(eat_ws(Rest), lists:reverse(Rels));

p_rel3(<< $\s, Rest/binary >>, Acc, Rels) ->
    p_rel3(eat_ws(Rest), <<>>, [ Acc | Rels ]);

p_rel3(<< C, Rest/binary >>, Acc, Rels) ->
    p_rel3(Rest, << Acc/binary, C >>, Rels).


p_rel4(<<>>, Rel) ->
    {rel, Rel, <<>>};

p_rel4(<< $;, Rest/binary >>, Rel) ->
    {rel, Rel, eat_ws(Rest)};

p_rel4(<< C, _Rest/binary >>, _Rel) ->
    throw({parse_error, {rel, C}}).


p_scheme(<<>>) ->
    throw({parse_error, {scheme, <<>>}});

p_scheme(<< $", Rest/binary >>) ->
    p_scheme2(Rest, <<>>);

p_scheme(<< C, _Rest/binary >>) ->
    throw({parse_error, {scheme, C}}).


p_scheme2(<< $", Rest/binary >>, Acc) ->
    p_scheme3(eat_ws(Rest), Acc);

p_scheme2(<< C, Rest/binary >>, Acc) ->
    p_scheme2(Rest, << Acc/binary, C >>).


p_scheme3(<< $;, Rest/binary >>, Uri) ->
    {scheme, Uri, eat_ws(Rest)};

p_scheme3(<< C, _Rest/binary >>, _Uri) ->
    throw({parse_error, {scheme, C}}).


p_link_type(<<>>, <<>>, []) ->
    throw({parse_error, {'link-type', <<>>}});

p_link_type(<<>>, Acc, Categories) ->
    {category, lists:reverse([ Acc | Categories ]), <<>>};

p_link_type(<< $\s, Rest/binary >>, Acc, Categories) ->
    p_link_type(eat_ws(Rest), <<>>, [ Acc | Categories ]);

p_link_type(<< C, Rest/binary >>, Acc, Categories) ->
    p_link_type(Rest, << Acc/binary, C >>, Categories).


p_number(<<>>, Acc) ->
    {integer, binary_to_integer(Acc), <<>>};

p_number(<< C, Rest/binary >>, Acc) when ?is_digit(C) ->
    p_number(Rest, << Acc/binary, C >>);

p_number(<< $., Rest/binary >>, Acc) ->
    p_float(Rest, << Acc/binary, $. >>);

p_number(<< C, Rest/binary >>, Acc) when C =:= $\s;
					 C =:= $;;
					 C =:= $, ->
    {integer, binary_to_integer(Acc), eat_ws(Rest)};

p_number(<< C, _Rest/binary >>, _Acc) ->
    throw({parse_error, {number, C}}).


p_float(<<>>, Acc) ->
    {float, binary_to_float(Acc), <<>>};

p_float(<< C, Rest/binary >>, Acc) when ?is_digit(C) ->
    p_float(Rest, << Acc/binary, C >>);

p_float(<< C, Rest/binary >>, Acc) when C =:= $\s;
					C =:= $;;
					C =:= $, ->
    {float, binary_to_float(Acc), Rest};

p_float(<< C, _Rest/binary >>, _Acc) ->
    throw({parse_error, {float, C}}).


p_attributes_def(<<>>) ->
    [];

p_attributes_def(<< $", Rest/binary >>) ->
    p_attributes_def2(Rest, []);

p_attributes_def(<< C, _Bin/binary >>) ->
    throw({parse_error, {attributes, C}}).


p_attributes_def2(<<>>, _Acc) ->
    throw({parse_error, {attributes, <<>>}});

p_attributes_def2(<< $", Rest/binary >>, Acc) ->
    p_attributes_def3(eat_ws(Rest), lists:reverse(Acc));

p_attributes_def2(<< $\s, Rest/binary >>, Acc) ->
    p_attributes_def2(Rest, Acc);

p_attributes_def2(<< C, Rest/binary >>, AttrList) when ?is_alpha(C);
						       ?is_digit(C) ->
    p_attribute_def(Rest, << C >>, #{ required => false, mutable => true }, AttrList);

p_attributes_def2(<< C, _Rest/binary >>, _AttrList) ->
    throw({parse_error, {attribute, C}}).


p_attributes_def3(<<>>, Acc) ->
    {attributes, Acc, <<>>};

p_attributes_def3(<< $;, Rest/binary >>, Acc) ->
    {attributes, Acc, eat_ws(Rest)};

p_attributes_def3(<< C, _Rest/binary >>, _Acc) ->
    throw({parse_error, {attributes, C}}).


p_attribute_def(<< $", Rest/binary >>, Acc, Attr, AttrList) ->
    {attributes, lists:reverse([ Attr#{ name => Acc } | AttrList ]), Rest};

p_attribute_def(<< $\s, Rest/binary >>, Acc, Attr, AttrList) ->
    p_attributes_def2(eat_ws(Rest), [ Attr#{ name => Acc } | AttrList ]);

p_attribute_def(<< ${, Rest/binary >>, Acc, Attr, AttrList) ->
    p_attribute_prop(eat_ws(Rest), Attr#{ name => Acc }, AttrList);

p_attribute_def(<< C, Rest/binary >>, Acc, Attr, AttrList) when ?is_alpha(C);
								?is_digit(C);
								C =:= $.;
								C =:= $-;
								C =:= $_ ->
    p_attribute_def(Rest, << Acc/binary, C >>, Attr, AttrList);

p_attribute_def(<< C, _Rest/binary >>, _Acc, _Attr, _AttrList) ->
    throw({parse_error, {attribute_name, C}}).


p_attribute_prop(<< "immutable", Rest/binary >>, Attr, AttrList) ->
    p_attribute_prop(eat_ws(Rest), Attr#{ mutable => false }, AttrList);

p_attribute_prop(<< "required", Rest/binary >>, Attr, AttrList) ->
    p_attribute_prop(eat_ws(Rest), Attr#{ required => true }, AttrList);

p_attribute_prop(<< $}, Rest/binary >>, Attr, AttrList) ->
    p_attributes_def2(eat_ws(Rest), [ Attr | AttrList ]);

p_attribute_prop(<< C, _Rest/binary >>, _Attr, _AttrList) ->
    throw({parse_error, {attribute_prop, C}}).


%%%
%%% Lex functions
%%%
p_alnum(<<>>, _End, Acc) ->
    {alnum, Acc, <<>>};

p_alnum(<< C, Rest/binary >>, End, Acc) ->
    case End(C) of
	true ->
	    {alnum, Acc, eat_ws(Rest)};
	false -> 
	    if ?is_alpha(C); 
	       ?is_digit(C); 
	       C =:= $-;
	       C =:= $_;
	       C =:= $. ->
		    p_alnum(Rest, End, << Acc/binary, C >>);
	       true ->
		    throw({parse_error, {alnum, C}})
	    end
    end.


p_qs(<<>>, _Begin, _End) ->
    {qs, <<>>, <<>>};

p_qs(<< Begin, Rest/binary >>, Begin, End) ->
    p_qs2(Rest, End, <<>>);

p_qs(<< C, _Rest/binary >>, _Begin, _End) ->
    throw({parse_error, {qs, C}}).


p_qs2(<<>>, _End, _Acc) ->
    throw({parse_error, {qs, <<>>}});

p_qs2(<< End, Rest/binary >>, End, Acc) ->
    {qs, Acc, eat_ws(Rest)};

p_qs2(<< C, Rest/binary >>, End, Acc) ->
    p_qs2(Rest, End, << Acc/binary, C >>).


p_uri_ref(Bin) ->
    {qs, UriRef, Rest} = p_qs(Bin, $<, $>),
    {'uri-ref', UriRef, Rest}.


p_string(<<>>, Acc) ->
    {string, Acc, <<>>};

p_string(<< $\\, $", Rest/binary >>, Acc) ->
    p_string(Rest, << Acc/binary, $" >>);

p_string(<< $", Rest/binary >>, Acc) ->
    {string, Acc, eat_ws(Rest)};

p_string(<< C, Rest/binary >>, Acc) ->
    p_string(Rest, << Acc/binary, C >>).    
    

p_class(<<>>) ->
    throw({parse_error, {class, <<>>}});

p_class(<< "action", Rest/binary >>) ->
    p_class2(Rest, action);

p_class(<< "\"action\"", Rest/binary >>) ->
    p_class2(Rest, action);

p_class(<< "mixin", Rest/binary >>) ->
    p_class2(Rest, mixin);

p_class(<< "\"mixin\"", Rest/binary >>) ->
    p_class2(Rest, mixin);

p_class(<< "kind", Rest/binary >>) ->
    p_class2(Rest, kind);

p_class(<< "\"kind\"", Rest/binary >>) ->
    p_class2(Rest, kind);

p_class(<< C, _Rest/binary >>) ->
    throw({parse_error, {class, C}}).


p_class2(<<>>, Class) ->
    {class, Class, <<>>};

p_class2(<< $\s, Rest/binary >>, Class) ->
    p_class2(Rest, Class);

p_class2(<< $;, Rest/binary >>, Class) ->
    {class, Class, eat_ws(Rest)};

p_class2(<< C, _Rest/binary >>, _Class) ->
    throw({parse_error, {class, C}}).


eat_ws(<< $\s, Rest/binary >>) ->
    eat_ws(Rest);

eat_ws(Rest) ->
    Rest.


filter_categories([], undefined, Mixins) ->
    {?link_kind_id, Mixins};

filter_categories([], Kind, Mixins) ->
    {Kind, Mixins};

filter_categories([ Id | Tail ], KindAcc, MixinsAcc) ->
    Category = occi_models:category(Id),
    case occi_category:class(Category) of
	kind ->
	    AddKind = fun (Kind, undefined) ->
			      Kind;
			  (Kind, _) ->
			      throw({parse_error, {kind_already_defined, Kind}})
		      end,
	    filter_categories(Tail, AddKind(Category, KindAcc), MixinsAcc);
	mixin ->
	    filter_categories(Tail, KindAcc, [ Id | MixinsAcc ])
    end.

%%%
%%% eunit
%%%
-ifdef(TEST).

attribute_test_() ->
    [
     ?_assertMatch({attributes, [
				 #{ name := <<"occi.example.attr1">>, mutable := true, required := false },
				 #{ name := <<"occi.example.attr2">>, mutable := true, required := false }
				], <<"etc">>},
		   p_attributes_def(<<"\"occi.example.attr1  occi.example.attr2 \"  ;   etc">>)),
     ?_assertMatch({attributes, [
				 #{ name := <<"occi.example.attr">>, mutable := true, required := false }
				], <<>>},
		   p_attributes_def(<<"\"occi.example.attr\"">>)),
     ?_assertMatch({attributes, [
				 #{ name := <<"occi.example.attr1">>, mutable := false, required := false },
				 #{ name := <<"occi.example.attr2">>, mutable := true, required := false }
				], <<>>},
		   p_attributes_def(<<"\"occi.example.attr1{immutable} occi.example.attr2\"">>)),
     ?_assertMatch({attributes, [
				 #{ name := <<"occi.example.attr">>, mutable := false, required := true }
				], <<>>},
		   p_attributes_def(<<"\"occi.example.attr{immutable required}\"">>)),
     ?_assertMatch({attributes, [
				 #{ name := <<"occi.example.attr">>, mutable := false, required := true }
				], <<>>},
		   p_attributes_def(<<"\"occi.example.attr{  required   immutable  }\"">>))
    ].

class_test_() ->
    [
     ?_assertThrow({parse_error, {class, $e}}, p_class(<<"action  else">>)),
     ?_assertMatch({class, action, <<"else">>}, p_class(<<"\"action\"  ;  else">>)),
     ?_assertMatch({class, mixin, <<"">>}, p_class(<<"\"mixin\"">>)),
     ?_assertMatch({class, kind, <<"">>}, p_class(<<"\"kind\"">>)),
     ?_assertThrow({parse_error, {class, $r}}, p_class(<<"resource">>))
    ].

eat_test_() ->
    [
     ?_assertMatch(<<>>, eat_ws(<<"       ">>)),
     ?_assertMatch(<<";">>, eat_ws(<<"    ;">>)),
     ?_assertMatch(<<";   else">>, eat_ws(<<"    ;   else">>))
    ].
-endif.
