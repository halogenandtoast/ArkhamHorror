module Arkham.Homebrew.DarkMatter.Treacheries.Infection (infection) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype Infection = Infection TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infection :: TreacheryCard Infection
infection = treachery Infection Cards.infection

{- | "As an additional cost to move or perform '[action]' abilities, take 1
damage."
-}
instance HasModifiersFor Infection where
  getModifiersFor (Infection a) =
    inThreatAreaGets
      a
      [ AdditionalCostToPerformAction (IsAction #move) payDamage
      , AdditionalCostToPerformAction (IsAction #activate) payDamage
      ]
   where
    payDamage = DamageCost (toSource a) YouTarget 1

-- | "Forced - At the end of your turn: Discard Infection."
instance HasAbilities Infection where
  getAbilities (Infection a) = [restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage Infection where
  runMessage msg t@(Infection attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Infection <$> liftRunMessage msg attrs
