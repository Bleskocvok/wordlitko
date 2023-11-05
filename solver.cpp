#include <iostream>     /*  */
#include <ostream>      /* ostream */
#include <fstream>      /* ifstream */
#include <cstdio>       /* printf */
#include <vector>       /* vector, erase_if */
#include <algorithm>    /* transform, sort, unique, count_if */
#include <string>       /* string, getline */
#include <string_view>  /* string_view */
#include <array>        /* array */
#include <stdexcept>    /* runtime_error */
#include <cctype>       /* isalpha, tolower */
#include <cstring>      /* strncmp, memset */
#include <utility>      /* pair */
#include <cstddef>      /* size_t */
#include <cmath>        /* ceil */


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
        for ( char cc : data )
            if ( c == cc )
                return true;
        return false;
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
    bool elsewhere = false;

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
        return false;
    }
};



inline bool accepts( const std::vector< clue_t >& clues, const word_t& w )
{
    for ( const auto& clue : clues )
    {
        if ( !clue.consistent( w ) )
            return false;

        if ( clue.col == color_t::GRAY
                && !clue.elsewhere
                && w.has( clue.c ))
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

    words.erase( std::unique( words.begin(), words.end() ), words.end() );

    return words;
}


inline void assign_elsewhere( std::vector< clue_t >& clues )
{
    bool yg_chars[ 256 ] = { 0 };

    for ( auto& clue : clues )
        if ( clue.col == color_t::GREEN || clue.col == color_t::YELLOW )
            yg_chars[ static_cast< unsigned char >( clue.c ) ] = true;

    for ( auto& clue : clues )
        if ( clue.col == color_t::GRAY )
            clue.elsewhere = yg_chars[ static_cast< unsigned char >( clue.c ) ];
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

    assign_elsewhere( clues );

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

    assign_elsewhere( clues );

    return clues;
}


inline void filter_accepting( std::vector< word_t >& words,
                              const std::vector< clue_t >& clues )
{
    std::erase_if( words, [&]( const auto& w ){ return !accepts( clues, w ); } );
}


inline long count_accepting( const std::vector< word_t >& words,
                             const std::vector< clue_t >& clues )
{
    return std::count_if( words.begin(), words.end(),
            [&]( const auto& w ){ return accepts( clues, w ); } );
}


inline float calculate_score( const word_t& guess,
                              const std::vector< word_t >& possible )
{
    if ( possible.empty() )
        return 1;

    auto scores = std::vector< long >{};
    scores.reserve( possible.size() );

    auto clues = std::vector< clue_t >{};

    for ( const auto& chosen : possible )
    {
        clues = get_clues( guess, chosen );
        scores.push_back( count_accepting( possible, clues ) );
    }

    std::sort( scores.begin(), scores.end() );

    if ( scores.size() < 2 )
        return scores.front();

    auto mid = scores.size() / 2 - 1;
    return std::ceil( ( scores[ mid ] + scores[ mid + 1 ] ) / 2.0 );
}


inline void order_best( std::vector< word_t >& words,
                        std::vector< word_t >& chosen )
{
    auto scores = std::vector< std::pair< float, word_t > >{};
    scores.reserve( words.size() );

    for ( const auto& w : words )
        scores.emplace_back( calculate_score( w, chosen ), w );

    std::sort( scores.begin(), scores.end() );

    for ( std::size_t i = 0; i < scores.size(); ++i )
        words[ i ] = scores[ i ].second;
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

    filter_accepting( database, clues );

    int each_nth = 1;
    if ( database.size() > 50 )
        each_nth = database.size() / 50;

    auto chosen = decltype( database ){};

    for ( std::size_t i = 0; i < database.size(); i += each_nth )
        chosen.push_back( database[ i ] );

    order_best( database, chosen );

    for ( auto w : database )
        std::cout << w << std::endl;

    return 0;
}
