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
#include <cstring>      /* strncmp, memset */


enum class color_t : int { GRAY, YELLOW, GREEN };

constexpr int WORD = 5;


struct word_t
{
    std::array< char, WORD > data;

    word_t() = default;

    word_t( const std::string& str )
    {
        if ( str.length() != WORD )
            throw std::runtime_error( "word_t" );

        for ( int i = 0; i < WORD; ++i )
            data[ i ] = str[ i ];
    }

    char operator[]( int i ) const { return data[ i ]; }
    char& operator[]( int i ) { return data[ i ]; }

    bool has( char c ) const
    {
        return std::find( data.begin(), data.end(), c ) != data.end();
    }

    friend auto operator==( const word_t& a, const word_t& b )
    {
        return a.data == b.data;
    }

    friend auto operator<=>( const word_t& a, const word_t& b )
    {
        return a.data <=> b.data;
    }

    friend std::ostream& operator<<( std::ostream& o, const word_t& w )
    {
        for ( int i = 0; i < WORD; ++i )
            o << w.data[ i ];
        return o;
    }
};


struct clue_t
{
    color_t col;
    int i;
    char c;

    friend std::ostream& operator<<( std::ostream& o, const clue_t& r )
    {
        auto bg = []( auto col )
        {
            switch ( col )
            {
                case color_t::GRAY:   return "\033[100m";
                case color_t::YELLOW: return "\033[43m";
                case color_t::GREEN:  return "\033[42m";
            }
            return "";
        };
        return o << "\033[30m" << bg( r.col ) << " " << r.c << " " << "\033[0m";
    }

    bool consistent( const word_t& w ) const
    {
        switch ( col )
        {
            case color_t::GREEN:  return w[ i ] == c;
            case color_t::YELLOW: return w[ i ] != c && w.has( c );
            case color_t::GRAY:   return w[ i ] != c;
        }
    }
};



inline bool accepts( const std::vector< clue_t >& clues, const word_t& w )
{
    static bool yg_chars[ 256 ] = { 0 };
    std::memset( yg_chars, 0, sizeof yg_chars );

    for ( const auto& clue : clues )
    {
        if ( clue.col == color_t::GREEN || clue.col == color_t::YELLOW )
            yg_chars[ static_cast< unsigned char >( clue.c ) ] = true;
    }

    auto gray = [&]( auto clue )
    {
        if ( yg_chars[ static_cast< unsigned char >( clue.c ) ] )
            return true;
        return !w.has( clue.c );
    };

    for ( const auto& clue : clues )
    {
        if ( !clue.consistent( w ) )
            return false;

        if ( clue.col == color_t::GRAY && !gray( clue ) )
            return false;
    }
    return true;
}

inline auto load_file( const std::string& filename ) -> std::vector< word_t >
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
            words.emplace_back( line);
    }

    std::sort( words.begin(), words.end(), []( const auto& a, const auto& b )
            { return std::strncmp( a.data.data(), b.data.data(), WORD ) < 0; } );

    std::unique( words.begin(), words.end() );

    return words;
}


inline auto parse_clues( std::string_view str ) -> std::vector< clue_t >
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

inline auto get_clues( word_t guess, word_t chosen ) -> std::vector< clue_t >
{
    auto color = [&]( char g, char c ) -> color_t
    {
        if ( g == c ) return color_t::GREEN;
        if ( chosen.has( g ) ) return color_t::YELLOW;
        return color_t::GRAY;
    };

    auto clues = std::vector< clue_t >{};
    for ( int i = 0; i < WORD; ++i )
        clues.push_back({ color( guess[ i ], chosen[ i ] ), i, guess[ i ] });
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
