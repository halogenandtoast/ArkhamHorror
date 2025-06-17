module Arkham.Story.Cards.SilverTwilightLodgeAllied (silverTwilightLodgeAllied) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target

newtype SilverTwilightLodgeAllied = SilverTwilightLodgeAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightLodgeAllied :: StoryCard SilverTwilightLodgeAllied
silverTwilightLodgeAllied = story SilverTwilightLodgeAllied Cards.silverTwilightLodgeAllied

instance HasAbilities SilverTwilightLodgeAllied where
  getAbilities (SilverTwilightLodgeAllied attrs) =
    [ limited (GroupLimit PerRound 2) $ mkAbility attrs 1 actionAbility
    , restricted attrs 2 (exists $ assetIs Assets.jewelOfSarnath <> AssetControlledBy Anyone)
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 5) Anywhere)
    ]

instance RunMessage SilverTwilightLodgeAllied where
  runMessage msg s@(SilverTwilightLodgeAllied attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 1 attrs
      pure s
    DiscardedTopOfEncounterDeck iid [card] _ (isTarget attrs -> True) -> do
      focusCards [card] do
        chooseOneM iid $ withI18n do
          labeled' "drawTheCard" $ addToHand iid (only card)
          labeled' "doNotDraw" nothing
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceCurrentAct (attrs.ability 2)
      pure s
    _ -> SilverTwilightLodgeAllied <$> liftRunMessage msg attrs
