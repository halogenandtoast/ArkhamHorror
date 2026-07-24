module Arkham.Story.Cards.RedeemAFormerColleague (redeemAFormerColleague) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
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
    [ restricted a 1 (exists $ edwin <> not_ (at_ YourLocation)) doubleActionAbility
    , restricted a 2 (exists $ edwin <> at_ YourLocation) parleyAction_
    , onlyOnce
        $ restricted
          a
          3
          ( exists
              $ edwin
              <> EnemyWithTokens (Static 3) Token.Redemption
              <> at_
                ( "Miskatonic University"
                    <> LocationWithAsset "Thomas Corrigan"
                    <> LocationWithAsset "Mary Zielinski"
                )
          )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage RedeemAFormerColleague where
  runMessage msg s@(RedeemAFormerColleague attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withMatch edwin \edwin' -> do
        readyThis edwin'
        withLocationOf iid $ enemyMoveTo (attrs.ability 1) edwin'
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withMatch edwin \edwin' -> do
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
        withMatch edwin \edwin' -> placeTokens (attrs.ability 2) edwin' Token.Redemption 1
      pure s
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      withMatch edwin \edwin' -> do
        withLocationOf edwin' $ createAssetAt_ Assets.edwinBennetAstuteAssociate . AtLocation
        removeFromGame edwin'
      lead <- getLead
      addToVictory lead attrs
      pure s
    _ -> RedeemAFormerColleague <$> liftRunMessage msg attrs
