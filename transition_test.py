import hy
from relang import (seq, star, non_greedy_star, named, maybe,
                    alternative, char, char_class, many)
from parse_interval import hms_pattern as HMS_PATTERN


def test_equivalence():
    pattern = (
        r'.*?'
         '(?P<url>'
             '(?:https?://)?(?:youtu\.be/'
             '|(?:www\.)?youtube\.com/watch)'
             '(?:\S)*[?&]t={}'.format(HMS_PATTERN) +
         ')'
         '(?:\s)+(?P<end>(?:\S)+)'.format(HMS_PATTERN)
    )

    new_pattern = seq(
        non_greedy_star(char.any),
        named('url',
            maybe('http', maybe("s"), "://"),
            alternative(
                'youtu\.be/',
                seq(maybe('www\.'), 'youtube\.com/watch')
            ),
            star(char.non_whitespace),
            seq(char_class('?&'), 't=', HMS_PATTERN),
        ),
        many(char.whitespace),
        named('end', many(char.non_whitespace))
    )
    
    assert pattern == new_pattern
