module Arkham.Homebrew.AgesUnwound.Locations.FrontHallway (frontHallway) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FrontHallway = FrontHallway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Unrevealed: "As an additional cost to enter Front Hallway, investigators at
Children's Playground must spend 3[per_investigator] clues as a group."

Matched by title rather than by def: Scenario VI gathers its own Children's
Playground (@:ages-unwound:171@), so @locationIs@ on Scenario III's copy would
quietly make the cost free there.
-}
frontHallway :: LocationCard FrontHallway
frontHallway =
  symbolLabel $ location FrontHallway Cards.frontHallway 3 (PerPlayer 2)
    & setCostToEnterUnrevealed (GroupClueCost (PerPlayer 3) "Children's Playground")

-- | "Haunted - If the Hound of Unmaking is in play and undamaged, place 1 damage on it"
instance HasAbilities FrontHallway where
  getAbilities (FrontHallway a) =
    extendRevealed1 a $ campaignI18n $ hauntedI "frontHallway.haunted" a 1

instance RunMessage FrontHallway where
  runMessage msg l@(FrontHallway attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      hounds <- select $ enemyIs Enemies.houndOfUnmaking <> EnemyWithDamage (EqualTo $ Static 0)
      for_ hounds \hound -> placeTokens (attrs.ability 1) hound #damage 1
      pure l
    _ -> FrontHallway <$> liftRunMessage msg attrs
