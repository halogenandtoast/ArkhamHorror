module Arkham.Asset.Assets.SigilOfThalarion2 (sigilOfThalarion2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype SigilOfThalarion2 = SigilOfThalarion2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilOfThalarion2 :: AssetCard SigilOfThalarion2
sigilOfThalarion2 = asset SigilOfThalarion2 Cards.sigilOfThalarion2

instance HasAbilities SigilOfThalarion2 where
  getAbilities (SigilOfThalarion2 a) =
    [ restricted a 1 (ControlsThis <> deepDream <> CanCreateDreamingCard You <> dreamingCardInHand)
        $ freeReaction (TurnEnds #when You)
    ]
   where
    {- Idol of Bokrug is deliberately not counted, however many dreams sit on it:
    it is "a dreaming card under your control" for MOVING dreams and for nothing
    else. -}
    deepDream = exists (DreamingCardControlledBy You <> DreamingCardWithDreams (atLeast 5))
    -- zone first: the query for an ExtendedCardMatcher starts from every card in
    -- the game and narrows left to right
    dreamingCardInHand = ExtendedCardExists (InHandOf ForPlay You <> CardWithDreaming AnyValue)

instance RunMessage SigilOfThalarion2 where
  runMessage msg a@(SigilOfThalarion2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      {- The turn is ending, so a dreaming card in hand has no window of its own
      left. "Play a card" is the permission; the same windows have to go to the
      playability check and to the play, or the two disagree. Fast and non-fast
      alike, since dreaming cards come both ways. -}
      let
        ws =
          nub
            $ windows'
            <> [ mkWhen Window.NonFast
               , mkWhen Window.FastPlayerWindow
               , mkWhen (Window.DuringTurn iid)
               ]
      cards <-
        filterM (getIsPlayable iid (attrs.ability 1) (UnpaidCost NoAction) ws)
          =<< select (inHandOf ForPlay iid <> CardWithDreaming AnyValue)
      when (notNull cards) do
        chooseOrRunOneM iid $ targets cards \card -> playCardPayingCostWithWindows iid card ws
      pure a
    _ -> SigilOfThalarion2 <$> liftRunMessage msg attrs
