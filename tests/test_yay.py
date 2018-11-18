# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

from unittest import skip

from mo_dots import wrap
from mo_json import value2json
from mo_testing.fuzzytestcase import FuzzyTestCase

from yay import parse, Word, Whitespace, text
from yay.patterns import Characters, Literal, Or, OneOrMore, Concat, Forward
from yay.util import letters, digits


class Test_YAY(FuzzyTestCase):

    def test_one_char(self):
        pattern = Literal("a")
        result = parse(pattern, "a")
        self.assertEqual(result, [{"start": 0, "stop": 1, "literal": "a"}])

    def test_literal(self):
        pattern = Literal("hello world")
        result = parse(pattern, "hello world")
        self.assertEqual(result, [{"start": 0, "stop": 11, "literal": "hello world"}])

    def test_characters(self):
        pattern = Characters(letters)
        result = parse(pattern, "H")
        self.assertEqual(
            result,
            [{"start": 0, "stop": 1, "data": "H"}]
        )

    def test_one_word(self):
        pattern = OneOrMore(Characters(letters))
        result = parse(pattern, "HelloWorld")
        self.assertEqual(
            result,
            [{
                "start": 0,
                "stop": 10,
                "sequence": [{"data": d} for d in "HelloWorld"]
            }]
        )

    def test_concat(self):
        pattern = Concat([Literal("("), Literal(")")])

        result = parse(pattern, "()")
        self.assertEqual(result, [{"start": 0, "stop": 2, "sequence": [{"literal": "("}, {"literal": ")"}]}])

    def test_concat_w_name(self):
        pattern = Concat([Literal("("), {"word": Word(letters)}, Literal(")")])

        result = parse(pattern, "(hello)")
        self.assertEqual(
            result,
            [{
                "start": 0,
                "stop": 7,
                "data": {"word": {"sequence": [{"data": w} for w in "hello"]}}
            }]
        )

    def test_or(self):
        pattern = Or([Literal("test"), Literal("stinker")])

        result = parse(pattern, "test")
        self.assertEqual(result, [{"start": 0, "stop": 4, "literal": "test"}])

        result = parse(pattern, "stinker")
        self.assertEqual(result, [{"start": 0, "stop": 7, "literal": "stinker"}])

    def test_whitespace(self):
        pattern = Concat([
            {"first": Word(letters)},
            {"rest": OneOrMore(Concat([
                Whitespace(),
                {"word": Word(letters)}
            ]))}
        ])
        #                        01234567890123
        result = parse(pattern, "this is a test")
        self.assertEqual(
            result,
            [
                {"start": 0, "stop": 14, "data": {
                    "first": {"start": 0, "stop": 4, "sequence": [{"data": w} for w in "this"]},
                    "rest": {"sequence": [
                        {"data": {"word": {"sequence": [{"data": w} for w in "is"]}}},
                        {"data": {"word": {"sequence": [{"data": w} for w in "a"]}}},
                        {"data": {"word": {"sequence": [{"data": w} for w in "test"]}}},
                    ]}
                }}
            ]
        )

    def test_forward_simple(self):
        expr = Forward("expr")
        sample = Or([
            OneOrMore(Characters(digits)),
            Concat([{"left": expr}, {"op": Literal("+")}, {"right": expr}]),
        ])
        expr << sample

        result = parse(expr, "2")
        expected = [{"start": 0, "stop": 1, "sequence": [
            {"start": 0, "stop": 1, "data": "2"}
        ]}]
        self.assertEqual(result, expected)

    def test_forward(self):
        expr = Forward("expr")
        sample = Or([
            OneOrMore(Characters(digits)),
            Concat([{"left": expr}, {"op": Literal("+")}, {"right": expr}]),
        ])
        expr << sample
        _ = value2json
        result = parse(expr, "2+4")
        expected = [{
            "data": {
                "left": {
                    "sequence": [{"data": "2", "start": 0, "stop": 1}],
                    "start": 0,
                    "stop": 1
                },
                "op": {"literal": "+", "start": 1, "stop": 2},
                "right": {
                    "sequence": [{"data": "4", "start": 2, "stop": 3}],
                    "start": 2,
                    "stop": 3
                }
            },
            "sequence": [
                {
                    "sequence": [{"data": "2", "start": 0, "stop": 1}],
                    "start": 0,
                    "stop": 1
                },
                {"literal": "+", "start": 1, "stop": 2},
                {
                    "sequence": [{"data": "4", "start": 2, "stop": 3}],
                    "start": 2,
                    "stop": 3
                }
            ],
            "start": 0,
            "stop": 3
        }]
        self.assertEqual(result, expected)

    def test_operators(self):
        expr = Forward("expr")
        sample = Or([
            OneOrMore(Characters(digits)).post_process(text),
            Concat([{"left": expr}, Whitespace(), {"op": Literal("*")}, Whitespace(), {"right": expr}]),
            Concat([{"left": expr}, Whitespace(), {"op": Literal("+")}, Whitespace(), {"right": expr}]),
        ])
        expr << sample

        result = wrap(parse(expr, "2 + 4 * 3"))

        self.assertEqual(result[1].data.left.sequence[0].data, "2")
        self.assertEqual(result[1].data.op.literal, "+")
        self.assertEqual(result[1].data.right.data.left.sequence[0].data, "4")
        self.assertEqual(result[1].data.right.data.op.literal, "*")
        self.assertEqual(result[1].data.right.data.right.sequence[0].data, "3")

        self.assertEqual(result[0].data.left.data.left.sequence[0].data, "2")
        self.assertEqual(result[0].data.left.data.op.literal, "+")
        self.assertEqual(result[0].data.left.data.right.sequence[0].data, "4")
        self.assertEqual(result[0].data.op.literal, "*")
        self.assertEqual(result[0].data.right.sequence[0].data, "3")

    @skip("broken")
    def test_operators_ordered(self):
        term = Forward("term")
        term << Or([
            OneOrMore(Characters(digits)).post_process(text),
            Concat([{"left": term}, Whitespace(), {"op": Literal("*")}, Whitespace(), {"right": term}]),
        ])

        expr = Forward("expr")
        expr << Or([
            term,
            Concat([{"left": expr}, Whitespace(), {"op": Literal("+")}, Whitespace(), {"right": expr}]),
        ])

        result = wrap(parse(expr, "2 + 4 * 3"))

        self.assertEqual(len(result), 1)
        self.assertEqual(result[0].data.left.sequence[0].data, "2")
        self.assertEqual(result[0].data.op.literal, "+")
        self.assertEqual(result[0].data.right.data.left.sequence[0].data, "4")
        self.assertEqual(result[0].data.right.data.op.literal, "*")
        self.assertEqual(result[0].data.right.data.right.sequence[0].data, "3")
