module Arkham.Story.Cards.RedeemAFormerColleague (redeemAFormerColleague) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Token qualified as Token

newtype RedeemAFormerColleague = RedeemAFormerColleague StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

redeemAFormerColleague :: StoryCard RedeemAFormerColleague
redeemAFormerColleague =
  storyWith RedeemAFormerColleague Cards.redeemAFormerColleague (flippedL .~ True) & persistStory

edwin :: EnemyMatcher
edwin = enemyIs Enemies.edwinBennetBitterAdversary

instance HasAbilities RedeemAFormerColleague where
  getAbilities (RedeemAFormerColleague a) =
    [ restricted a 1 (exists edwin) $ actionAbilityWithCost (ActionCost 1)
    , restricted a 2 (exists (edwin <> EnemyAt YourLocation)) parleyAction_
    , restricted
        a
        3
        ( exists
            $ edwin
            <> EnemyWithTokens (Static 3) Token.Redemption
            <> EnemyAt
              ( LocationWithTitle "Miskatonic University"
                  <> LocationWithAsset (AssetWithTitle "Thomas Corrigan")
                  <> LocationWithAsset (AssetWithTitle "Mary Zielinski")
              )
        )
        $ forced AnyWindow
    ]

instance RunMessage RedeemAFormerColleague where
  runMessage msg s@(RedeemAFormerColleague attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectForMaybeM edwin \edwin' -> do
        readyThis edwin'
        getLocationOf iid >>= traverse_ \lid -> push $ EnemyMove edwin' lid
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectForMaybeM (edwin <> enemyAtLocationWith iid) \edwin' -> do
        sid <- getRandom
        chooseOneM iid $ withI18n do
          chooseTest #willpower 3 $ parley sid iid (attrs.ability 2) edwin' #willpower (Fixed 3)
          chooseTest #intellect 3 $ parley sid iid (attrs.ability 2) edwin' #intellect (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      total <- getPlayerCount
      investigatorClues <- selectWithField InvestigatorClues UneliminatedInvestigator
      let totalClues = sum (map snd investigatorClues)
      when (totalClues >= total) do
        spendCluesAsAGroup (map fst investigatorClues) total
        selectForMaybeM edwin \edwin' ->
          placeTokens (attrs.ability 2) edwin' Token.Redemption 1
      pure s
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      selectForMaybeM edwin \edwin' -> do
        mLocation <- getLocationOf edwin'
        removeFromGame edwin'
        for_ mLocation \lid ->
          createAssetAt_ Assets.edwinBennetAstuteAssociate (AtLocation lid)
        lead <- getLead
        addToVictory lead attrs
      pure s
    _ -> RedeemAFormerColleague <$> liftRunMessage msg attrs
