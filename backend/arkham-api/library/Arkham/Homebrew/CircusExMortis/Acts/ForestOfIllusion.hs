module Arkham.Homebrew.CircusExMortis.Acts.ForestOfIllusion (forestOfIllusion) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Token
import Arkham.Trait (Trait (Woods))

newtype ForestOfIllusion = ForestOfIllusion ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forestOfIllusion :: ActCard ForestOfIllusion
forestOfIllusion =
  act (1, A) ForestOfIllusion Cards.forestOfIllusion Nothing

-- A Woods location whose text the act's free ability may blank. You cannot use
-- the ability at Circular Grove while it has 1 or more clues (its printed text).
validWoodsLocation :: LocationMatcher
validWoodsLocation =
  LocationWithTrait Woods
    <> not_ (locationIs Locations.moonlitForestCircularGrove <> LocationWithAnyClues)

instance HasAbilities ForestOfIllusion where
  getAbilities (ForestOfIllusion a) =
    [ restricted a 1 (youExist $ at_ validWoodsLocation)
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    , restricted a 2 (EachUndefeatedInvestigator $ at_ $ locationIs Locations.circusEncampment)
        $ Objective
        $ forced (RoundEnds #when)
    ]

instance RunMessage ForestOfIllusion where
  runMessage msg a@(ForestOfIllusion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loc <- getJustLocation iid
      -- Treat the location's printed text box as blank (except Traits and
      -- Victory) for the remainder of the game; a horror marks the reminder.
      gameModifier (attrs.ability 1) loc Blank
      placeTokens (attrs.ability 1) loc Horror 1
      -- Gradually unworking the illusions opens a route toward the camp.
      camp <- selectJust $ locationIs Locations.circusEncampment
      connectBothWays camp loc
      -- TODO(homebrew): Misty Marsh's "additional cost to activate this ability
      -- here: seal a moon token" is not modeled as a pre-cost.
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Each undefeated investigator reached Circus Encampment: the illusions
      -- were bypassed (Resolution 2).
      push R2
      pure a
    _ -> ForestOfIllusion <$> liftRunMessage msg attrs
