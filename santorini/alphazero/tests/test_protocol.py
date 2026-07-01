from __future__ import annotations

import unittest

import numpy as np

from santorini_az.protocol import place_workers


class ProtocolTest(unittest.TestCase):
    def test_deterministic_placement_matches_haskell_preference(self) -> None:
        self.assertEqual(place_workers([]), [[2, 2], [4, 4]])
        self.assertEqual(place_workers([[2, 2], [4, 4]]), [[2, 4], [4, 2]])

    def test_random_placement_is_distinct_and_avoids_opponent(self) -> None:
        occupied = [[2, 2], [4, 4]]
        placed = place_workers(occupied, np.random.default_rng(9))
        self.assertEqual(len(placed), 2)
        self.assertNotEqual(placed[0], placed[1])
        self.assertTrue(all(worker not in occupied for worker in placed))
        self.assertTrue(
            all(1 <= coordinate <= 5 for worker in placed for coordinate in worker)
        )


if __name__ == "__main__":
    unittest.main()
