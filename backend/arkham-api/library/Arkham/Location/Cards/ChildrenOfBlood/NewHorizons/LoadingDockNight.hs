module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.LoadingDockNight (loadingDockNight) where

import Arkham.Ability hiding (resignAction)
import Arkham.I18n
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype LoadingDockNight = LoadingDockNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loadingDockNight :: LocationCard LoadingDockNight
loadingDockNight = symbolLabel $ location LoadingDockNight Cards.loadingDockNight 3 (PerPlayer 1)

instance HasAbilities LoadingDockNight where
  getAbilities (LoadingDockNight a) =
    let doorForcedOpen = toResultDefault False a.meta
     in extendRevealed a
          $ [skillTestAbility $ restricted a 1 Here actionAbility]
          <> [resignAction a | doorForcedOpen]

instance RunMessage LoadingDockNight where
  runMessage msg l@(LoadingDockNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      canDoom <- selectAny $ not_ (AgendaWithModifier CannotPlaceDoomOnThis)
      chooseOneM iid $ withI18n $ countVar 1 do
        labeledValidate' canDoom "placeAgendaDoom" do
          placeDoomOnAgendaBy (attrs.ability 1) 1
          skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
        labeled "doNotPlaceDoom" nothing
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      pure $ LoadingDockNight $ setMeta True attrs
    _ -> LoadingDockNight <$> liftRunMessage msg attrs
