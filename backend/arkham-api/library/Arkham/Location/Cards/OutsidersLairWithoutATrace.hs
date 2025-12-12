module Arkham.Location.Cards.OutsidersLairWithoutATrace (outsidersLairWithoutATrace) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Outsider))

newtype OutsidersLairWithoutATrace = OutsidersLairWithoutATrace LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outsidersLairWithoutATrace :: LocationCard OutsidersLairWithoutATrace
outsidersLairWithoutATrace =
  symbolLabel
    $ locationWith
      OutsidersLairWithoutATrace
      Cards.outsidersLairWithoutATrace
      5
      (PerPlayer 2)
      connectsToAdjacent

instance HasModifiersFor OutsidersLairWithoutATrace where
  getModifiersFor (OutsidersLairWithoutATrace a) = do
    modifySelect
      a
      (enemyAt a <> EnemyWithTrait Outsider)
      [EnemyFight 1, EnemyEvade 1, RemoveKeyword Keyword.Aloof]

instance HasAbilities OutsidersLairWithoutATrace where
  getAbilities (OutsidersLairWithoutATrace a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> ExtendedCardCount (EqualTo $ Static 1) HollowedCard
            <> exists (HollowedCard <> basic (cardIs Assets.theRedGlovedManHeWasAlwaysThere))
        )
      $ FastAbility Free

instance RunMessage OutsidersLairWithoutATrace where
  runMessage msg l@(OutsidersLairWithoutATrace attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> exposedInShadows iid attrs $ assignDamage iid (attrs.ability (-1)) 1
          MiddlePosition -> do
            cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
            unless (null cards) do
              exposedInShadows iid attrs $ chooseTargetM iid cards $ hollow iid
          RightPosition -> exposedInShadows iid attrs $ assignHorror iid (attrs.ability (-1)) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iids <- select $ investigatorAt attrs
      theRedGlovedMan <- createAssetAt Assets.theRedGlovedManHeWasAlwaysThere Unplaced
      chooseOrRunOneM iid $ targets iids (`takeControlOfAsset` theRedGlovedMan)
      pure l
    _ -> OutsidersLairWithoutATrace <$> liftRunMessage msg attrs
