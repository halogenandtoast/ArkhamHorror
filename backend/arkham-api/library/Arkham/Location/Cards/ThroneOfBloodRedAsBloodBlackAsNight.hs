module Arkham.Location.Cards.ThroneOfBloodRedAsBloodBlackAsNight (throneOfBloodRedAsBloodBlackAsNight) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement

newtype ThroneOfBloodRedAsBloodBlackAsNight = ThroneOfBloodRedAsBloodBlackAsNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throneOfBloodRedAsBloodBlackAsNight :: LocationCard ThroneOfBloodRedAsBloodBlackAsNight
throneOfBloodRedAsBloodBlackAsNight =
  locationWith
    ThroneOfBloodRedAsBloodBlackAsNight
    Cards.throneOfBloodRedAsBloodBlackAsNight
    4
    (PerPlayer 3)
    (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 4) "Gothic Set")

instance HasModifiersFor ThroneOfBloodRedAsBloodBlackAsNight where
  getModifiersFor (ThroneOfBloodRedAsBloodBlackAsNight a) = do
    modifySelectWhen a a.revealed (enemyIs Enemies.theContessaNeedlesslySmug <> at_ (be a)) [CannotMove]

instance HasAbilities ThroneOfBloodRedAsBloodBlackAsNight where
  getAbilities (ThroneOfBloodRedAsBloodBlackAsNight a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #when You (be a)
      , fastAbility a 2 (clueCost 1) (Here <> exists (at_ (be a) <> EnemyCanBeDamagedBySource (a.ability 2)))
      ]

instance RunMessage ThroneOfBloodRedAsBloodBlackAsNight where
  runMessage msg l@(ThroneOfBloodRedAsBloodBlackAsNight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      contessa <-
        selectOne (enemyIs Enemies.theContessaNeedlesslySmug) >>= \case
          Just contessa -> do
            healAllDamage (attrs.ability 1) contessa
            enemyMoveTo (attrs.ability 1) contessa attrs
            pure contessa
          Nothing -> do
            card <- fetchCard Enemies.theContessaNeedlesslySmug
            createEnemyAt card attrs.id

      createAssetAt_ (SetAsideCard Assets.accursedCapeShroudOfChaos) (AttachedToEnemy contessa)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ at_ (be attrs) <> EnemyCanBeDamagedBySource (attrs.ability 2)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1
      pure l
    _ -> ThroneOfBloodRedAsBloodBlackAsNight <$> liftRunMessage msg attrs
