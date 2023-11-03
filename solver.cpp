#include <iostream>     /*  */
#include <ostream>      /* ostream */
#include <fstream>      /* ifstream */
#include <cstdio>       /* printf */
#include <vector>       /* vector, erase_if */
#include <algorithm>    /* transform, sort, unique */
#include <string>       /* string, getline */
#include <string_view>  /* string_view */
#include <array>        /* array */
#include <stdexcept>    /* runtime_error */
#include <cctype>       /* isalpha, tolower */
#include <cstring>      /* strncmp */


constexpr int WORD = 5;

using word_t = std::array< char, WORD >;


auto wout( const word_t& w )
{
    std::printf( "%.*s", WORD, w.data() );
}

word_t mkword( const std::string& str )
{
    if ( str.length() != WORD )
        throw std::runtime_error( "mkword" );

    word_t res;
    for ( int i = 0; i < WORD; ++i )
        res[ i ] = str[ i ];
    return res;
}

enum class color_t : int { GRAY, YELLOW, GREEN };

struct clue_t
{
    color_t col;
    int i;
    char c;

    friend std::ostream& operator<<( std::ostream& o, const clue_t& r )
    {
        return o << "clue_t{ " << int( r.col ) << ", " << r.i << ", " << r.c << " }";
    }
};


auto load_file( const std::string& filename ) -> std::vector< word_t >
{
    auto words = std::vector< word_t >{};
    auto in = std::ifstream( filename );
    if ( !in )
        throw std::runtime_error( "opening file" );

    auto line = std::string{};
    while ( in )
    {
        std::getline( in, line );

        std::erase_if( line, []( unsigned char c )
                { return !std::isalpha( c ); } );

        if ( line.empty() )
            continue;

        std::transform( line.begin(), line.end(), line.begin(),
                []( unsigned char c ){ return std::tolower( c ); } );

        if ( line.length() == WORD )
            words.push_back( mkword( line ) );
    }

    std::sort( words.begin(), words.end(), []( const auto& a, const auto& b )
            { return std::strncmp( a.data(), b.data(), WORD ) < 0; } );

    std::unique( words.begin(), words.end() );

    return words;
}


auto parse_clues( std::string_view str ) -> std::vector< clue_t >
{
    std::vector< clue_t > clues;

    auto low = []( unsigned char c ){ return std::tolower( c ); };
    auto get = [&]()
    {
        char c = low( str.front() );
        str.remove_prefix( 1 );
        return c;
    };

    int i = 0;
    while ( !str.empty() )
    {
        char c = get();
        color_t col;
        switch ( c )
        {
            case '.': ++i; continue;
            case ' ':      continue;
            case '\n':     continue;
            case '\r':     continue;
            case '!': col = color_t::GRAY;   c = get(); break;
            case '^': col = color_t::YELLOW; c = get(); break;
            default:  col = color_t::GREEN;             break;
        }

        clues.push_back({ .col = col, .i = i, .c = c });
    }

    return clues;
}


int main( int argc, char** argv )
{
    if ( argc < 3 )
    {
        std::printf( "usage: %s CLUES DATABASE_PATH\n", argv[ 0 ] );
        return 1;
    }

    auto database = load_file( argv[ 2 ] );
    auto clues = parse_clues( argv[ 1 ] );

    for ( auto r : clues )
        std::cout << r << std::endl;

    return 0;
}
