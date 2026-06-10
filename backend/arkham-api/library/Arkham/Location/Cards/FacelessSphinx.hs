module Arkham.Location.Cards.FacelessSphinx (facelessSphinx) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FacelessSphinx = FacelessSphinx LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

facelessSphinx :: LocationCard FacelessSphinx
facelessSphinx = location FacelessSphinx Cards.facelessSphinx 0 (PerPlayer 1)

instance HasModifiersFor FacelessSphinx where
  getModifiersFor (FacelessSphinx a) = do
    n <- getStrengthOfTheAbyss
    modifySelf a [ShroudModifier n]

instance HasAbilities FacelessSphinx where
  getAbilities (FacelessSphinx a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> youExist (InvestigatorWithClues $ atLeast 2)) actionAbility

instance RunMessage FacelessSphinx where
  runMessage msg l@(FacelessSphinx attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) iid attrs #clue 2
      removeStrengthOfTheAbyss 1
      pure l
    _ -> FacelessSphinx <$> liftRunMessage msg attrs
