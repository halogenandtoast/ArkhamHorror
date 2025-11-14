module Arkham.Story.Cards.PackageDelivery (packageDelivery) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Query (allInvestigators, getJustLocationByName)
import Arkham.Location.Types (Field (LocationCardsUnderneath))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy
import Arkham.Trait (Trait (Ally, Casino, Item))

newtype PackageDelivery = PackageDelivery StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

packageDelivery :: StoryCard PackageDelivery
packageDelivery = story PackageDelivery Cards.packageDelivery

instance HasModifiersFor PackageDelivery where
  getModifiersFor (PackageDelivery a) = case a.placement of
    AttachedToEnemy e -> modified_ a e [ForcePatrol "Vault Door", RemoveTrait Casino]
    _ -> pure ()

instance HasAbilities PackageDelivery where
  getAbilities (PackageDelivery a) = case a.placement of
    AttachedToEnemy e -> [mkAbility a 1 $ SilentForcedAbility $ EnemyEnters #when "Vault Door" (be e)]
    AtLocation _ -> [restricted a 2 OnSameLocation actionAbility]
    _ -> []

instance RunMessage PackageDelivery where
  runMessage msg s@(PackageDelivery attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember DeliveredADecoyPackage
      investigators <- allInvestigators
      leadChooseOneM do
        portraits investigators \iid ->
          search
            iid
            (attrs.ability 1)
            iid
            [fromDeck]
            (basic $ hasAnyTrait [Ally, Item])
            (defer attrs IsNotDraw)

      case attrs.placement of
        AttachedToEnemy e -> toDiscard (attrs.ability 1) e
        _ -> pure ()
      vaultDoor <- getJustLocationByName "Vault Door"
      pure $ PackageDelivery $ attrs & placementL .~ AtLocation vaultDoor
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      vaultDoor <- getJustLocationByName "Vault Door"
      cards <- field LocationCardsUnderneath vaultDoor
      for_ cards $ putCardIntoPlay iid
      removeStory attrs
      pure s
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      vaultDoor <- getJustLocationByName "Vault Door"
      chooseOneM iid $ cardsLabeled cards $ placeUnderneath vaultDoor . only
      pure s
    _ -> PackageDelivery <$> liftRunMessage msg attrs
