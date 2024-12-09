module Arkham.Investigator.Cards.TonyMorgan (tonyMorgan, TonyMorgan (..)) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Fight
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (defaultWindows)
import Control.Lens (over)
import Data.Data.Lens (biplate)

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype TonyMorgan = TonyMorgan (InvestigatorAttrs `With` Meta)
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

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
  getAbilities (TonyMorgan (attrs `With` _)) =
    [ doesNotProvokeAttacksOfOpportunity
      $ restrictedAbility
        attrs
        1
        (Self <> exists (EnemyWithBounty <> oneOf [CanFightEnemy source, CanEngageEnemy source]))
      $ ActionAbility [] mempty
    | BountyAction `notElem` map additionalActionType (investigatorUsedAdditionalActions attrs)
    ]
   where
    source = toSource attrs

instance HasChaosTokenValue TonyMorgan where
  getChaosTokenValue iid ElderSign (TonyMorgan (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage TonyMorgan where
  runMessage msg i@(TonyMorgan (attrs `With` meta)) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let windows' = defaultWindows iid

      -- we should move these to a helper function to reuse between the InvestigatorRunner and here
      actions <- withModifiers attrs (toModifiers attrs [ActionCostModifier (-1), BountiesOnly]) $ do
        map (over biplate (`decreaseActionCost` 1))
          . filter (any (`elem` [#fight, #engage]) . abilityActions)
          <$> getActions iid windows'
      playableCards <- withModifiers attrs (toModifiers attrs [BountiesOnly]) $ do
        filter (any (`elem` [#fight, #engage]) . cdActions . toCardDef)
          <$> getPlayableCards attrs (UnpaidCost NoAction) windows'

      canPlay <- canDo (toId attrs) #play
      player <- getPlayer iid

      push
        $ AskPlayer
        $ chooseOne player
        $ [ targetLabel (toCardId c) [InitiatePlayCard iid c Nothing NoPayment windows' False, DoStep 1 msg]
          | canPlay
          , c <- playableCards
          ]
        <> map ((\f -> f windows' [] [DoStep 1 msg]) . AbilityLabel iid) actions
      pure
        $ TonyMorgan
        . (`with` Meta True)
        $ attrs
        & (usedAdditionalActionsL %~ (AdditionalAction "Tony Morgan" (toSource attrs) BountyAction :))
    ChooseFightEnemy choose | choose.investigator == toId attrs -> do
      bountiesOnly <- hasModifier attrs BountiesOnly
      let matcherF = if bountiesOnly then (<> EnemyWithBounty) else id
      result <-
        runMessage (ChooseFightEnemy $ choose {chooseFightEnemyMatcher = matcherF choose.matcher}) attrs
      pure $ TonyMorgan . (`with` Meta False) $ result
    ChooseEngageEnemy iid source mTarget enemyMatcher isAction | iid == toId attrs -> do
      bountiesOnly <- hasModifier iid BountiesOnly
      let matcherF = if bountiesOnly then (<> EnemyWithBounty) else id
      result <-
        runMessage
          (ChooseEngageEnemy iid source mTarget (matcherF enemyMatcher) isAction)
          attrs
      pure $ TonyMorgan . (`with` Meta False) $ result
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      pure $ TonyMorgan $ attrs `with` Meta False
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      bountyContracts <- selectJust $ assetIs Assets.bountyContracts
      push $ AddUses #elderSign bountyContracts Bounty 1
      pure i
    ResetGame -> TonyMorgan . (`with` Meta False) <$> runMessage msg attrs
    _ -> TonyMorgan . (`with` meta) <$> runMessage msg attrs
