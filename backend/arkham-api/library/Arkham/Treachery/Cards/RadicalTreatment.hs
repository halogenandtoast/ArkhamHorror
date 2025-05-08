module Arkham.Treachery.Cards.RadicalTreatment (radicalTreatment) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RadicalTreatment = RadicalTreatment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

radicalTreatment :: TreacheryCard RadicalTreatment
radicalTreatment = treachery RadicalTreatment Cards.radicalTreatment

instance HasAbilities RadicalTreatment where
  getAbilities (RadicalTreatment a) =
    [ restricted a 1 (youExist $ ControlsAsset $ assetIs Assets.danielChesterfield)
        $ forced
        $ RoundEnds #when
    , skillTestAbility
        $ restricted a 2 (OnSameLocation <> youExist (at_ $ locationIs Locations.infirmary)) actionAbility
    ]

instance RunMessage RadicalTreatment where
  runMessage msg t@(RadicalTreatment attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 2
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      hasClaspOfBlackOnyx <- iid <=~> ControlsAsset (assetIs Assets.claspOfBlackOnyx)
      when hasClaspOfBlackOnyx $ skillTestModifier sid (attrs.ability 2) sid (Difficulty (-3))
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 2) attrs kind (Fixed 6)
      pure t
    PassedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      addToVictory attrs
      pure t
    _ -> RadicalTreatment <$> liftRunMessage msg attrs
