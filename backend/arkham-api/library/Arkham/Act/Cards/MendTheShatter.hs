module Arkham.Act.Cards.MendTheShatter (mendTheShatter) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard, PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Shattered))

newtype MendTheShatter = MendTheShatter ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mendTheShatter :: ActCard MendTheShatter
mendTheShatter = act (4, A) MendTheShatter Cards.mendTheShatter Nothing

instance HasAbilities MendTheShatter where
  getAbilities = actAbilities \a ->
    [ skillTestAbility
        $ restricted a 1 (youExist $ at_ (LocationWithoutClues <> withTrait Shattered)) actionAbility
    , restricted
        a
        2
        (exists $ "Relic of Ages" <> AssetWithCardsUnderneath (LengthIs $ AtLeast $ StaticWithPerPlayer 1 1))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage MendTheShatter where
  runMessage msg a@(MendTheShatter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
      selectEach (colocatedWith iid) \iid' -> moveTo attrs iid' aPocketInTime
      withLocationOf iid \lid -> do
        card <- field LocationCard lid
        relic <- selectJust $ AssetWithTitle "Relic of Ages"
        selectEach (UnengagedEnemy <> enemyAt lid) \eid -> enemyMoveTo attrs eid aPocketInTime
        removeLocation lid
        placeUnderneath relic (only card)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      isRespossessThePast <- selectAny $ assetIs Assets.relicOfAgesRepossessThePast
      push $ if isRespossessThePast then R5 else R1
      pure a
    _ -> MendTheShatter <$> liftRunMessage msg attrs
