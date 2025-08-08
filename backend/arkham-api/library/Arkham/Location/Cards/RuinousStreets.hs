module Arkham.Location.Cards.RuinousStreets (ruinousStreets) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype RuinousStreets = RuinousStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinousStreets :: LocationCard RuinousStreets
ruinousStreets = locationWith RuinousStreets Cards.ruinousStreets 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RuinousStreets where
  getAbilities (RuinousStreets a) =
    extendRevealed
      a
      [ restricted a 1 (DuringSkillTest $ SkillTestAt (be a)) $ forced $ RevealChaosToken #after You #frost
      , restricted a 2 (exists $ LocationWithDiscoverableCluesBy You <> ConnectedFrom (be a))
          $ freeReaction (DiscoveringLastClue #after You (be a))
      ]

instance RunMessage RuinousStreets where
  runMessage msg l@(RuinousStreets attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      ls <- select $ LocationWithDiscoverableCluesBy (InvestigatorWithId iid) <> ConnectedFrom (be attrs)
      chooseTargetM iid ls $ discoverAt NotInvestigate iid (attrs.ability 2) 1
      pure l
    _ -> RuinousStreets <$> liftRunMessage msg attrs
