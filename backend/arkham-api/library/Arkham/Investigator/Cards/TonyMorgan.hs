module Arkham.Investigator.Cards.TonyMorgan (tonyMorgan) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Constants
import Arkham.Fight
import Arkham.Helpers.Action
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (defaultWindows)
import Control.Lens (over)
import Data.Data.Lens (biplate)

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype TonyMorgan = TonyMorgan (InvestigatorAttrs `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator TonyMorgan where
  investigatorFromAttrs = TonyMorgan . (`with` Meta False)

tonyMorgan :: InvestigatorCard TonyMorgan
tonyMorgan =
  investigator (TonyMorgan . (`with` Meta False)) Cards.tonyMorgan
    $ Stats {health = 9, sanity = 5, willpower = 2, intellect = 3, combat = 5, agility = 2}

instance HasModifiersFor TonyMorgan where
  getModifiersFor (TonyMorgan (a `With` meta)) =
    modifySelf a
      $ GiveAdditionalAction (AdditionalAction "Tony Morgan" (toSource a) BountyAction)
      : [BountiesOnly | active meta]

instance HasAbilities TonyMorgan where
  getAbilities (TonyMorgan (a `With` _)) =
    [ doesNotProvokeAttacksOfOpportunity
        $ selfAbility
          a
          NonActivateAbility
          (exists (EnemyWithBounty <> oneOf [CanFightEnemyWith (SourceOwnedBy $ be a), CanEngageEnemy source]))
          (ActionAbility [] Free)
    | BountyAction `notElem` map additionalActionType a.usedAdditionalActions
    ]
   where
    source = toSource a

instance HasChaosTokenValue TonyMorgan where
  getChaosTokenValue iid ElderSign (TonyMorgan (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage TonyMorgan where
  runMessage msg i@(TonyMorgan (attrs `With` meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) NonActivateAbility -> do
      let windows' = defaultWindows iid

      -- we should move these to a helper function to reuse between the InvestigatorRunner and here
      actions <- withModifiers attrs (toModifiers attrs [ActionCostModifier (-1), BountiesOnly]) $ do
        map (over biplate (`decreaseActionCost` 1))
          . filter (any (`elem` [#fight, #engage]) . abilityActions)
          <$> getActions iid windows'
      playableCards <- withModifiers attrs (toModifiers attrs [BountiesOnly]) $ do
        filter (any (`elem` [#fight, #engage]) . cdActions . toCardDef)
          <$> getPlayableCards attrs iid (UnpaidCost NoAction) windows'

      chooseOneM iid do
        whenM (canDo (toId attrs) #play) do
          targets playableCards \c -> do
            playCardPayingCost iid c
            doStep 1 msg
        for_ actions \ab -> abilityLabeled iid ab (doStep 1 msg)
      pure
        $ TonyMorgan
        . (`with` Meta True)
        $ attrs
        & (usedAdditionalActionsL %~ (AdditionalAction "Tony Morgan" (toSource attrs) BountyAction :))
    ChooseFightEnemy c | c.investigator == toId attrs -> do
      bountiesOnly <- hasModifier attrs BountiesOnly
      let matcherF = if bountiesOnly then (<> EnemyWithBounty) else id
      result <-
        liftRunMessage (ChooseFightEnemy $ c {chooseFightEnemyMatcher = matcherF c.matcher}) attrs
      pure $ TonyMorgan . (`with` Meta False) $ result
    ChooseEngageEnemy iid source mTarget enemyMatcher isAction | iid == toId attrs -> do
      bountiesOnly <- hasModifier iid BountiesOnly
      let matcherF = if bountiesOnly then (<> EnemyWithBounty) else id
      result <-
        liftRunMessage
          (ChooseEngageEnemy iid source mTarget (matcherF enemyMatcher) isAction)
          attrs
      pure $ TonyMorgan . (`with` Meta False) $ result
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) NonActivateAbility) -> do
      pure $ TonyMorgan $ attrs `with` Meta False
    ElderSignEffect (is attrs -> True) -> do
      mBountyContracts <- selectOne $ assetIs Assets.bountyContracts
      for_ mBountyContracts \bountyContracts ->
        placeTokens ElderSign bountyContracts Bounty 1
      pure i
    ResetGame -> TonyMorgan . (`with` Meta False) <$> liftRunMessage msg attrs
    _ -> TonyMorgan . (`with` meta) <$> liftRunMessage msg attrs
