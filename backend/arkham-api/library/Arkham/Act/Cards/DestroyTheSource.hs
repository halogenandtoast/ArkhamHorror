module Arkham.Act.Cards.DestroyTheSource (destroyTheSource) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (AssetDefeated)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword (Keyword (Alert))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Trait (Trait (Dinosaur))

newtype DestroyTheSource = DestroyTheSource ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

destroyTheSource :: ActCard DestroyTheSource
destroyTheSource = act (3, A) DestroyTheSource Cards.destroyTheSource Nothing

instance HasModifiersFor DestroyTheSource where
  getModifiersFor (DestroyTheSource a) = do
    modifySelect
      a
      (EnemyWithTitle "Possessed Extra")
      [HealthModifier 2, EnemyEvade 2, AddKeyword Alert, AddTrait Dinosaur]

instance HasAbilities DestroyTheSource where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
    , restricted
        a
        2
        (exists $ VictoryDisplayCardMatch $ basic $ cardIs Assets.staffOfTheSerpentRelicOfThePast)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage DestroyTheSource where
  runMessage msg a@(DestroyTheSource attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) EncounterDeckTarget [fromTopOfDeck 6] (basic #enemy) (DrawFound iid 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      locations <- select $ LocationWithMostInvestigators Anywhere
      chooseOrRunOneM iid do
        targets locations \lid -> for_ cards (`createEnemyAt_` lid)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R3
      pure a
    _ -> DestroyTheSource <$> liftRunMessage msg attrs
