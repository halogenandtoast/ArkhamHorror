module Arkham.Location.Cards.Tindalos (tindalos) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Trait (Trait (Portal, Scientist))

newtype Tindalos = Tindalos LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tindalos :: LocationCard Tindalos
tindalos = location Tindalos Cards.tindalos 5 (Static 0)

instance HasModifiersFor Tindalos where
  getModifiersFor (Tindalos a) = do
    self <- modifySelf a [ConnectedToWhen (be a) (LocationWithTrait Portal)]
    others <-
      modifySelect a (not_ $ LocationWithId a.id) [ConnectedToWhen (LocationWithTrait Portal) (be a)]
    pure $ self <> others

instance HasAbilities Tindalos where
  getAbilities (Tindalos a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> DuringTurn You) (FastAbility Free)
      , restricted a 2 (Here <> exists (SetAsideCardMatch $ CardWithTrait Scientist)) actionAbility
      ]

instance RunMessage Tindalos where
  runMessage msg l@(Tindalos attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      portals <- select $ LocationWithTrait Portal <> CanEnterLocation (InvestigatorWithId iid)
      chooseTargetM iid portals (moveTo (attrs.ability 1) iid)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ withI18n do
        chooseTest #combat 2 $ beginSkillTest sid iid (attrs.ability 2) iid #combat (Fixed 2)
        chooseTest #agility 2 $ beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      scientists <- getAbductedScientists
      focusCards scientists \unfocus -> do
        chooseOneM iid do
          for_ scientists \card -> cardLabeled card do
            unfocus
            aid <- createAssetAt card (AtLocation attrs.id)
            exhaustThis aid
      pure l
    _ -> Tindalos <$> liftRunMessage msg attrs
