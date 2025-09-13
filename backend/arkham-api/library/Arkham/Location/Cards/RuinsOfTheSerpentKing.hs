module Arkham.Location.Cards.RuinsOfTheSerpentKing (ruinsOfTheSerpentKing) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Token

newtype RuinsOfTheSerpentKing = RuinsOfTheSerpentKing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfTheSerpentKing :: LocationCard RuinsOfTheSerpentKing
ruinsOfTheSerpentKing = location RuinsOfTheSerpentKing Cards.ruinsOfTheSerpentKing 4 (PerPlayer 2)

instance HasModifiersFor RuinsOfTheSerpentKing where
  getModifiersFor (RuinsOfTheSerpentKing a) = do
    whenUnrevealed a $ blockedWhen a (pure $ hasToken Seal a.tokens)

instance HasAbilities RuinsOfTheSerpentKing where
  getAbilities (RuinsOfTheSerpentKing a) =
    scenarioI18n
      $ extendRevealed
        a
        [ withI18nTooltip "ruinsOfTheSerpentKing.exhaustEnemy"
            $ restricted a 1 Here
            $ actionAbilityWithCost (ChooseEnemyCostAndMaybeFieldClueCost (at_ $ be a) EnemyFight)
        , withI18nTooltip "ruinsOfTheSerpentKing.attemptToWrestStaff"
            $ skillTestAbility
            $ restricted a 2 (Here <> exists (AssetWithPlacement $ AtLocation a.id)) actionAbility
        ]

instance RunMessage RuinsOfTheSerpentKing where
  runMessage msg l@(RuinsOfTheSerpentKing attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ (chosenEnemyPayment -> menemy) -> do
      for_ menemy exhaustThis
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      combinationSkillTest sid iid (attrs.ability 2) iid [#willpower, #combat] (Fixed 6)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      staff <- selectJust $ assetIs Assets.staffOfTheSerpentRelicOfThePast
      takeControlOfAsset iid staff
      pure l
    _ -> RuinsOfTheSerpentKing <$> liftRunMessage msg attrs
