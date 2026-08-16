module Arkham.Homebrew.DarkMatter.FacedownThreatAreaSpec (spec) where

import Arkham.Homebrew.DarkMatter.Traits (pattern Liminal)
import Arkham.Matcher
import Arkham.Placement
import TestImport.New

{- | Lost Quantum's face-down encounter cards sit in a threat area unresolved and
out of play. An enemy card among them (a defeated Quantum Phantom) must not
answer plain enemy queries: The Quantum Maelstrom's "move each non-[[Liminal]]
enemy" was dragging face-down phantoms out of the threat area and into play.
-}
spec :: Spec
spec = describe "face-down cards in a threat area" do
  it "are not matched by a plain enemy query" . gameTest $ \self -> do
    enemy <- testEnemy
    run $ PlaceEnemy (toId enemy) (FacedownInThreatArea (toId self))
    assertNone $ not_ (EnemyWithTrait Liminal)

  it "are still matched by their face-down placement" . gameTest $ \self -> do
    enemy <- testEnemy
    run $ PlaceEnemy (toId enemy) (FacedownInThreatArea (toId self))
    assertAny $ EnemyWithPlacement (FacedownInThreatArea (toId self))
