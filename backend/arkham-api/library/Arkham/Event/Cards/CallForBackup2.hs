module Arkham.Event.Cards.CallForBackup2 (callForBackup2, CallForBackup2 (..)) where

import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Damage
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Movement

newtype CallForBackup2 = CallForBackup2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callForBackup2 :: EventCard CallForBackup2
callForBackup2 = event CallForBackup2 Cards.callForBackup2

control :: HasGame m => ClassSymbol -> m Bool
control k = selectAny (ControlledBy You <> basic (CardWithClass k))

componentLabel :: Targetable target => GameTokenType -> target -> [Message] -> UI Message
componentLabel component (toTarget -> target) = case target of
  InvestigatorTarget iid' -> ComponentLabel (InvestigatorComponent iid' component)
  AssetTarget aid -> ComponentLabel (AssetComponent aid component)
  _ -> error "unhandled target"

healableAsset :: Sourceable source => source -> DamageType -> LocationMatcher -> AssetMatcher
healableAsset (toSource -> source) hType loc = HealableAsset source hType $ at_ loc <> AssetControlledBy (affectsOthers Anyone)

damageComponentLabel :: (Sourceable source, Targetable target) => target -> source -> UI Message
damageComponentLabel (toTarget -> thing) (toSource -> source) = componentLabel DamageToken thing [HealDamage thing source 1]

horrorComponentLabel :: (Sourceable source, Targetable target) => target -> source -> UI Message
horrorComponentLabel (toTarget -> thing) (toSource -> source) = componentLabel HorrorToken thing [HealDamage thing source 1]

instance RunMessage CallForBackup2 where
  runMessage msg e@(CallForBackup2 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      push $ DoStep 1 msg
      pure e
    DoStep 1 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      hasRogue <- control Rogue
      locations <- getAccessibleLocations iid attrs
      when (hasRogue && not (null locations)) $ do
        player <- getPlayer iid
        chooseOne
          iid
          [ Label
              "Move to a connecting location"
              [Msg.chooseOne player $ targetLabels locations (only . Move . move attrs iid)]
          , Label "Do not Move" []
          ]
      push $ DoStep 2 msg'
      pure e
    DoStep 2 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      hasGuardian <- control Guardian
      enemies <- select $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource (toSource attrs)
      canDealDamage <- iid <=~> InvestigatorWithoutModifier CannotDealDamage
      when (hasGuardian && not (null enemies) && canDealDamage) do
        chooseOne iid $ targetLabels enemies $ only . Msg.nonAttackEnemyDamage attrs 1
      push $ DoStep 3 msg'
      pure e
    DoStep 3 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      hasSeeker <- control Seeker
      when hasSeeker $ discoverAtYourLocation NotInvestigate iid attrs 1
      push $ DoStep 4 msg'
      pure e
    DoStep 4 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      hasMystic <- control Mystic
      horrorCards <-
        (<>)
          <$> selectTargets (HealableInvestigator (toSource attrs) #horror $ colocatedWith iid)
          <*> selectTargets (healableAsset attrs #horror $ locationWithInvestigator iid)
      when (hasMystic && notNull horrorCards) do
        chooseOne iid [horrorComponentLabel target attrs | target <- horrorCards]
      push $ DoStep 5 msg'
      pure e
    DoStep 5 (PlayThisEvent iid (is attrs -> True)) -> do
      hasSurvivor <- control Survivor
      damageCards <-
        (<>)
          <$> selectTargets (HealableInvestigator (toSource attrs) #damage $ colocatedWith iid)
          <*> selectTargets (healableAsset attrs #damage $ locationWithInvestigator iid)
      when (hasSurvivor && notNull damageCards) do
        chooseOne iid [damageComponentLabel target attrs | target <- damageCards]
      pure e
    _ -> CallForBackup2 <$> liftRunMessage msg attrs
