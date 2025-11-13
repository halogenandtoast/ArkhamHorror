module Arkham.Asset.Assets.TheGrifterUnpracticed (theGrifterUnpracticed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card.CardCode
import Arkham.Helpers.Ref (targetToSource)
import Arkham.I18n
import Arkham.Matcher hiding (InvestigatorEliminated)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheGrifterUnpracticed = TheGrifterUnpracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGrifterUnpracticed :: AssetCard TheGrifterUnpracticed
theGrifterUnpracticed = asset TheGrifterUnpracticed Cards.theGrifterUnpracticed

instance HasAbilities TheGrifterUnpracticed where
  getAbilities (TheGrifterUnpracticed a) = [controlled_ a 1 $ triggered (ScenarioEvent #when (Just You) "checkGameIcons") (exhaust a)]

getCheckGameIcons :: [Window] -> CheckGameIcons
getCheckGameIcons = \case
  [] -> error "missing heretic"
  ((windowType -> Window.ScenarioEvent "checkGameIcons" _ value) : _) -> toResult value
  (_ : xs) -> getCheckGameIcons xs

instance RunMessage TheGrifterUnpracticed where
  runMessage msg a@(TheGrifterUnpracticed attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCheckGameIcons -> check) _ -> do
      focusCards check.cards $ scenarioI18n do
        chooseTargetM iid check.cards \card -> do
          mpc <- toPlayingCard card
          for_ mpc \pc -> do
            let
              setPlayingCard :: ReverseQueue m => PlayingCard -> m ()
              setPlayingCard npc =
                abilityModifier
                  (AbilityRef (targetToSource check.target) 1)
                  (attrs.ability 1)
                  (CardCodeTarget $ toCardCode card)
                  $ ScenarioModifierValue "setPlayingCard"
                  $ toJSON npc
            chooseOneM iid do
              labeled' "theGrifter.rank" do
                toFour <- capture $ setPlayingCard $ PlayingCard Four pc.suit
                toFive <- capture $ setPlayingCard $ PlayingCard Five pc.suit
                toSix <- capture $ setPlayingCard $ PlayingCard Six pc.suit
                toSeven <- capture $ setPlayingCard $ PlayingCard Seven pc.suit
                toEight <- capture $ setPlayingCard $ PlayingCard Eight pc.suit
                toNine <- capture $ setPlayingCard $ PlayingCard Nine pc.suit
                toTen <- capture $ setPlayingCard $ PlayingCard Ten pc.suit
                toJack <- capture $ setPlayingCard $ PlayingCard Jack pc.suit
                toQueen <- capture $ setPlayingCard $ PlayingCard Queen pc.suit
                toKing <- capture $ setPlayingCard $ PlayingCard King pc.suit
                toAce <- capture $ setPlayingCard $ PlayingCard Ace pc.suit
                chooseOneDropDown
                  iid
                  [ (ikey' "label.rank.Four", Run toFour)
                  , (ikey' "label.rank.Five", Run toFive)
                  , (ikey' "label.rank.Six", Run toSix)
                  , (ikey' "label.rank.Seven", Run toSeven)
                  , (ikey' "label.rank.Eight", Run toEight)
                  , (ikey' "label.rank.Nine", Run toNine)
                  , (ikey' "label.rank.Ten", Run toTen)
                  , (ikey' "label.rank.Jack", Run toJack)
                  , (ikey' "label.rank.Queen", Run toQueen)
                  , (ikey' "label.rank.King", Run toKing)
                  , (ikey' "label.rank.Ace", Run toAce)
                  ]
              labeled' "theGrifter.suit" do
                toHearts <- capture $ setPlayingCard $ PlayingCard pc.rank Hearts
                toDiamonds <- capture $ setPlayingCard $ PlayingCard pc.rank Diamonds
                toClubs <- capture $ setPlayingCard $ PlayingCard pc.rank Clubs
                toSpades <- capture $ setPlayingCard $ PlayingCard pc.rank Spades
                chooseOneDropDown
                  iid
                  [ (ikey' "label.suit.Hearts", Run toHearts)
                  , (ikey' "label.suit.Diamonds", Run toDiamonds)
                  , (ikey' "label.suit.Clubs", Run toClubs)
                  , (ikey' "label.suit.Spades", Run toSpades)
                  ]
              labeled' "theGrifter.color" do
                setPlayingCard
                  $ PlayingCard pc.rank
                  $ case pc.suit of
                    Hearts -> Clubs
                    Diamonds -> Clubs
                    Clubs -> Hearts
                    Spades -> Hearts
      pure a
    InvestigatorEliminated _ -> pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theGrifterPracticed
      pure a
    _ -> TheGrifterUnpracticed <$> liftRunMessage msg attrs
