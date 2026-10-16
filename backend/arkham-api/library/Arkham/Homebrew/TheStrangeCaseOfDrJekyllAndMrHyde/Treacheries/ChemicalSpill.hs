{- | "Revelation - Attach Chemical Spill to your location."

The toll is a __Forced__ ability on the would-move window, not a movement cost:
it is paid when the move is announced and never cancels it.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.ChemicalSpill (
  chemicalSpill,
) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype ChemicalSpill = ChemicalSpill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chemicalSpill :: TreacheryCard ChemicalSpill
chemicalSpill = treachery ChemicalSpill Cards.chemicalSpill

instance HasAbilities ChemicalSpill where
  getAbilities (ChemicalSpill a) =
    [ scenarioI18n
        $ withI18nTooltip "chemicalSpill.toll"
        $ mkAbility a 1
        $ forced
        $ WouldMove #when You #any (LocationWithTreachery (be a)) Anywhere
    , scenarioI18n
        $ withI18nTooltip "chemicalSpill.discard"
        $ mkAbility a 2
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage ChemicalSpill where
  runMessage msg t@(ChemicalSpill attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ scenarioI18n $ scope "chemicalSpill" do
        labeled "loseAction" $ loseActions iid (attrs.ability 1) 1
        labeled "takeDamageAndHorror" $ assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> ChemicalSpill <$> liftRunMessage msg attrs
