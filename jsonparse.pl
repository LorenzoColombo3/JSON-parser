/* Colombo Lorenzo 885895
 * Santin Simone 886116 */

/*
 *  jsonparse
 */

jsonparse( {}, jsonobj( [] ) ).

jsonparse( JSONAtom, ParsedJSON ) :-
    atom( JSONAtom ),
    atom_string( JSONAtom, JSONString ),
    jsonparse( JSONString, ParsedJSON ).

jsonparse( JSONString, jsonobj( Members ) ) :-
    string( JSONString ),
    jsonobj( JSONString, Members ).

/*
 * jsonobj
 */

jsonobj( {}, _ ).

jsonobj( JSONString, Members ) :- 
    removeBrackets( JSONString, Content ),
    parsemember( [ Content ], Members ).

parsemember( [], [] ).

parsemember( [ Content ], [ Member ] ) :-
    parsepair( Content, Member ).

parsemember( [ Content ], [ Member | OtherMembers ] ) :-
    Content =.. [ ',',  Pair | Rest ],
    parsepair( Pair, Member ),
    parsemember( Rest, OtherMembers ).

/*
 *	scompone la coppia attributo valore
 */
parsepair( Pair, ( Attribute, Value ) ) :-
    Pair =.. [ ':', Attribute, RawValue ],
    string( Attribute ),
    parseValue( RawValue, Value ).

parseValue( RawValue, Value ) :-
    RawValue =.. [ _ | _ ],
    term_string( RawValue, RawValueString ),
    atom_string( RawValueAtom, RawValueString ),
    jsonparse( RawValueAtom, Value ).

/*
 *	controllo se value è una stringa 
 */
parseValue( Value, Value ) :-
    string( Value ).

/*
 *	controllo se value è un numero 
 */
parseValue( Value, Value ) :-
    number( Value ).
    
/*
 *	controllo se value è un boolean o null
 */
parseValue( true, true ).
parseValue( false, false ).
parseValue( null, null ).

/*
 * jsonarray
 */

jsonparse( [], jsonarray( [] ) ).

jsonparse( JSONString, jsonarray( ParsedElements ) ) :-
    string( JSONString ),
    term_string( Array, JSONString ),
    jsonarray( Array, ParsedElements ).

jsonarray( [], [] ).
jsonarray( [ Element | Elements ], [ ParsedElement | OtherParsedElements ] ) :-
    parseValue( Element, ParsedElement ),
    jsonarray( Elements, OtherParsedElements ).

/*
 * gestione JSONString
 */

removeBrackets( JSONString, Content ) :-
    term_string( JSON, JSONString ), 
    JSON =.. [ {} , Content ].


/*
 *  jsonaccess
 */

jsonaccess( _, [], _ ) :- false.


jsonaccess( ParsedObject, [ Attribute ], Result ) :-
    jsonaccess( ParsedObject, Attribute, Result ).

jsonaccess( ParsedObject, Attribute, Result ) :-
    string( Attribute ),
    ParsedObject =.. [ jsonobj, Members ],
    searchField( Members, Attribute, Field ),
    Field = ( Attribute, Result ).

searchField( [], _, _ ) :- false.

searchField( Members, Attribute, Field ) :-
    Members = [ ( OtherAttribute, Value ) | OtherMembers ],
    ( OtherAttribute = Attribute ->
        ( ( OtherAttribute, Value ) = Field )
        ;
        ( searchField( OtherMembers, Attribute, Field ) ) ).


jsonaccess( ParsedObject, [ Attribute, Index ], Result ) :-
    string( Attribute ),
    number( Index ),
    ParsedObject =.. [ jsonobj , Members ],
    searchField( Members, Attribute, Field ),
    Field = ( Attribute, Value ),
    Value =.. [ jsonarray, Array ],
    searchValue( Array, Index, Result ).

searchValue( [], _, _ ) :- false.

searchValue( [ Value | OtherValues ], RegressiveCounter, Result ) :-
    ( RegressiveCounter = 0 ->
        ( Value = Result )
        ;
        ( NewRegressiveCounter is RegressiveCounter - 1,
         searchValue( OtherValues, NewRegressiveCounter, Result ) ) ).

parsedjson( "[]", jsonarray( [] ) ).
parsedjson( JSONString, ParsedArray ) :-
    ParsedArray =.. [ jsonarray, Array ],
    arrayjson( Array, JSONTemp ),
    string_concat( "[", JSONTemp, JSONTemp2 ),
    string_concat( JSONTemp2, "]", JSONString ).


arrayjson( [ Element | OtherElements ], JSONString ) :-
    valuejson(Element, JSONTemp),
    ( OtherElements = [] ->
        ( JSONString = JSONTemp )
        ;
        ( arrayjson( OtherElements, JSONTemp2 ),
         string_concat( JSONTemp, ",", JSONTemp3 ),
         string_concat( JSONTemp3, JSONTemp2, JSONString ) ) ).

parsedjson( "{}", jsonobj( [] ) ).

parsedjson( JSONString, ParsedObject ) :-
    ParsedObject =.. [ jsonobj, ParsedMembers ],
    memberjson( ParsedMembers, MembersJSON ),
    tostring( MembersJSON, Result ),
    string_concat( "{", Result, ResultTemp ),
    string_concat( ResultTemp, "}", JSONString ).

memberjson( [],[] ).

memberjson( [ ParsedMember | OtherParsedMembers ], 
            [ MemberJSON | OtherMembersJSON ] ) :-
    ParsedMember = ( Attribute, ParsedValue ),
    string( Attribute ),
    valuejson( ParsedValue, ValueJSON ),
    string_concat( "\"", Attribute, MemberJSONTemp1 ),
    string_concat( MemberJSONTemp1 ,"\"", MemberJSONTemp2 ),
    string_concat( MemberJSONTemp2, ":", MemberJSONTemp3 ),
    string_concat( MemberJSONTemp3, ValueJSON, MemberJSON ),
    memberjson( OtherParsedMembers, OtherMembersJSON ).

valuejson( Value, String ) :-
    string( Value ),
    string_concat( "\"", Value, StringTemp1 ),
    string_concat( StringTemp1, "\"", String ).

valuejson( Value, Value ) :-
    number( Value ).

valuejson( true, true ).
valuejson( false, false ).
valuejson( null, null ).

valuejson( ParsedValue, ValueJSON ) :-
    parsedjson( ValueJSON,ParsedValue ).

tostring( [ MemberJSON | OtherMembersJSON ] , Result ) :-
    ( OtherMembersJSON = [] ->
        ( Result = MemberJSON )
        ;
        ( string_concat( MemberJSON, ",", ResultTemp1 ),
         tostring( OtherMembersJSON, ResultTemp2 ),
         string_concat( ResultTemp1, ResultTemp2, Result ) ) ).

/*
 *  scrittura su file (jsondump)
 */

jsondump( ParsedJSON, Filename ) :-
    atom( Filename ),
    parsedjson( JSONString, ParsedJSON ),
    atom_string( AtomJSON, JSONString ),
    open( Filename, write, Output ),
    write( Output, AtomJSON ),
    close( Output ).

/*
 *	lettura da file (jsonread)
 */

jsonread( Filename, ParsedJSON ) :-
    atom( Filename ),
    open( Filename, read, Input ),
    read_string( Input, _, JSONString ),
    atom_string( AtomJSON, JSONString ),
    jsonparse( AtomJSON, ParsedJSON ),
    close( Input ).