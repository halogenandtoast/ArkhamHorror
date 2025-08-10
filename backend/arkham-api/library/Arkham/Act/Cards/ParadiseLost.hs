module Arkham.Act.Cards.ParadiseLost (paradiseLost) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard, PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Shattered))

newtype ParadiseLost = ParadiseLost ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradiseLost :: ActCard ParadiseLost
paradiseLost = act (4, A) ParadiseLost Cards.paradiseLost Nothing

instance HasAbilities ParadiseLost where
  getAbilities = actAbilities \a ->
    [ skillTestAbility
        $ restricted a 1 (youExist $ at_ (LocationWithoutClues <> LocationWithTrait Shattered)) actionAbility
    , restricted
        a
        2
        (exists $ "Relic of Ages" <> AssetWithCardsUnderneath (HasCard $ cardIs Locations.valusia))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage ParadiseLost where
  runMessage msg a@(ParadiseLost attrs) = runQueueT $ case msg of
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
      push R2
      pure a
    _ -> ParadiseLost <$> liftRunMessage msg attrs
