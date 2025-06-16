module Arkham.Story.Cards.TMGSilverTwilightLodgeAllied (
  tmgSilverTwilightLodgeAllied,
  TMGSilverTwilightLodgeAllied(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TMGSilverTwilightLodgeAllied = TMGSilverTwilightLodgeAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tmgSilverTwilightLodgeAllied :: StoryCard TMGSilverTwilightLodgeAllied
tmgSilverTwilightLodgeAllied = story TMGSilverTwilightLodgeAllied Cards.tmgSilverTwilightLodgeAllied

instance HasAbilities TMGSilverTwilightLodgeAllied where
  getAbilities (TMGSilverTwilightLodgeAllied attrs) =
    [ limitedAbility (GroupLimit PerRound 2)
        $ mkAbility attrs 1
        $ ActionAbility Nothing Free
    , restrictedAbility attrs 2
        ( exists
            $ assetIs Assets.jewelOfSarnath
            <> AssetControlledBy Anyone
        )
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 5) Anywhere)
    ]

instance RunMessage TMGSilverTwilightLodgeAllied where
  runMessage msg s@(TMGSilverTwilightLodgeAllied attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ DiscardTopOfEncounterDeck iid 1 (attrs.ability 1) (Just $ toTarget attrs)
      pure s
    DiscardedTopOfEncounterDeck iid [card] _ (isTarget attrs -> True) -> do
      player <- getPlayer iid
      focusCards [EncounterCard card] do
        chooseOne player
          [ Label "Draw the card" [AddToHand iid [toCard card]]
          , Label "Do not draw" []
          ]
        unfocusCards
      pure s
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      actId <- getCurrentAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure s
    _ -> TMGSilverTwilightLodgeAllied <$> liftRunMessage msg attrs
