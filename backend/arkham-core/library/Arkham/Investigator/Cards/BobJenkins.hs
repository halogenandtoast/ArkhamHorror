module Arkham.Investigator.Cards.BobJenkins (bobJenkins, BobJenkins (..)) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Card
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Game.Helpers (getPlayableCards, toModifiers, withModifiers)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (defaultWindows, mkWhen)
import Arkham.Window qualified as Window

newtype BobJenkins = BobJenkins InvestigatorAttrs
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bobJenkins :: InvestigatorCard BobJenkins
bobJenkins =
  investigator
    BobJenkins
    Cards.bobJenkins
    Stats {health = 6, sanity = 8, willpower = 2, intellect = 4, combat = 3, agility = 3}

instance HasModifiersFor BobJenkins where
  getModifiersFor target (BobJenkins a) | a `is` target = do
    pure
      $ toModifiers
        a
        [GiveAdditionalAction (AdditionalAction "Bob Jenkins" (toSource a) BobJenkinsAction)]
  getModifiersFor _ _ = pure []

instance HasAbilities BobJenkins where
  getAbilities (BobJenkins attrs) =
    [ restrictedAbility
      attrs
      1
      ( Self
          <> PlayableCardExists
            UnpaidCost
            (basic (#asset <> #item) <> InHandOf (affectsOthers $ InvestigatorAt YourLocation))
      )
      $ ActionAbility [] mempty
    | BobJenkinsAction `notElem` map additionalActionType (investigatorUsedAdditionalActions attrs)
    ]

instance HasChaosTokenValue BobJenkins where
  getChaosTokenValue iid ElderSign (BobJenkins attrs) | iid == toId attrs = do
    n <- selectCount $ assetControlledBy iid <> #item
    pure $ ChaosTokenValue ElderSign (PositiveModifier n)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage BobJenkins where
  runMessage msg i@(BobJenkins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let windows' = defaultWindows iid

      investigators <- select (affectsOthers $ colocatedWith iid)

      playableCards <- concatForM investigators $ \iid' -> do
        cards <- select $ InHandOf (InvestigatorWithId iid') <> basic (#asset <> #item)
        withModifiers
          iid'
          (toModifiers attrs [CanSpendResourcesOnCardFromInvestigator (InvestigatorWithId iid) AnyCard])
          $ do
            withModifiers iid (toModifiers attrs $ map (PlayableCardOf iid') cards) $ do
              filter (`cardMatch` (CardWithType AssetType <> #item))
                <$> getPlayableCards attrs UnpaidCost windows'

      chooseOne iid
        $ [ targetLabel (toCardId c) [HandleTargetChoice iid (attrs.ability 1) (CardIdTarget $ toCardId c)]
          | c <- playableCards
          ]
      pure
        $ BobJenkins
        $ attrs
        & (usedAdditionalActionsL %~ (AdditionalAction "Bob Jenkins" (toSource attrs) BobJenkinsAction :))
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      let windows' = defaultWindows iid
      c <- getCard cid
      case toCardOwner c of
        Nothing -> error "No Card Owner"
        Just owner -> do
          cardResolutionModifier
            c
            (attrs.ability 1)
            owner
            (CanSpendResourcesOnCardFromInvestigator (InvestigatorWithId iid) AnyCard)
          cardResolutionModifier c (attrs.ability 1) attrs (PlayableCardOf owner c)
          checkWindows [mkWhen (Window.PlayCard iid c)]
          push $ PayCardCost iid c windows'
          pure i
    _ -> BobJenkins <$> lift (runMessage msg attrs)
