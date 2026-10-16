{- | Your Other Half is a bonded investigator card: Duplicitous Drug sets your
own card aside and puts this one into play in its place.

The swap is the engine's 'TransfiguredForm', which routes abilities, modifiers,
chaos token values, health, sanity and all four skills through this card, so the
1/1 and the 4/4/4/4 come from the builder below and are stated nowhere else.
What it does not move is the damage and horror, which Duplicitous Drug stashes
on the way in; the reversion here puts that stash back, discarding whatever Your
Other Half accumulated -- "with all tokens on it" names the damage and horror the
card was set aside with, in both directions. Resources and clues are never
stashed, so they simply carry across both ways.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Investigators.YourOtherHalf (
  yourOtherHalf,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.HasQueue (removeAllMessagesMatching, replaceMessageMatching)
import Arkham.Constants (pattern ActAdvancement)
import Arkham.Helpers.Card (drawThisCardFrom)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Assets.DuplicitousDrug (setAsideTokens)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Investigators qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (..), InvestigatorForm (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token qualified as Token

newtype YourOtherHalf = YourOtherHalf InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

yourOtherHalf :: InvestigatorCard YourOtherHalf
yourOtherHalf =
  investigator YourOtherHalf Cards.yourOtherHalf
    $ Stats {health = 1, sanity = 1, willpower = 4, intellect = 4, combat = 4, agility = 4}

instance HasModifiersFor YourOtherHalf where
  getModifiersFor (YourOtherHalf a) = do
    -- "choose to advance the current act" is the act's own [fast] objective,
    -- ability 999 on the act
    acts <- select AnyAct
    modifySelf a
      $ CannotSpendClues
      : [CannotTriggerAbilityMatching (AbilityIs (ActSource act) ActAdvancement) | act <- acts]

instance HasAbilities YourOtherHalf where
  getAbilities (YourOtherHalf a) =
    scenarioI18n
      [ withI18nTooltip "yourOtherHalf.suppress"
          $ restricted a 1 Self
          $ freeReaction
          $ DrawCard #when You (CanCancelRevelationEffect You $ basic WeaknessCard) AnyDeck
      , withI18nTooltip "yourOtherHalf.revert"
          $ restricted a 2 Self
          $ forced
          $ InvestigatorWouldBeDefeated #when (ByAnyOf [ByDamage, ByHorror]) You
      ]

instance HasChaosTokenValue YourOtherHalf where
  getChaosTokenValue iid ElderSign (YourOtherHalf attrs) | iid == attrs.id = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage YourOtherHalf where
  runMessage msg i@(YourOtherHalf attrs) = runQueueT $ case msg of
    ElderSignEffect iid | iid == attrs.id -> do
      mDrug <- selectOne $ assetIs Assets.duplicitousDrug <> assetControlledBy iid
      for_ mDrug \drug -> push $ AddUses #elderSign drug #dose 1
      pure i
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelCardDraw (attrs.ability 1) card
      placeUnderneath iid [card]
      pure i
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- the defeat is replaced rather than cancelled: drop the damage that was
      -- about to land and re-check once we are our own card again
      lift do
        replaceMessageMatching
          (\case InvestigatorWhenDefeated _ iid' -> iid == iid'; _ -> False)
          \case
            InvestigatorWhenDefeated source _ -> [Msg.checkDefeated source iid]
            _ -> error "invalid match"
        removeAllMessagesMatching \case
          AssignDamage (InvestigatorTarget iid') _ -> iid == iid'
          AssignedDamage (InvestigatorTarget iid') _ _ _ -> iid == iid'
          _ -> False

      -- "remove Your Other Half from the game": gone from the bonded cards, so
      -- a second dose can never search it up again
      bonded <- fieldMap InvestigatorBondedCards (filterCards (cardIs Cards.yourOtherHalf)) iid
      for_ bonded $ push . RemoveCard . toCardId
      push $ SetInvestigatorForm iid RegularForm

      let weaknesses = filterCards WeaknessCard attrs.cardsUnderneath
      pushAll $ concatMap (\card -> drawThisCardFrom iid card Nothing) weaknesses

      wounds <- setAsideTokens iid
      pure
        . YourOtherHalf
        $ attrs
          { investigatorTokens =
              foldr
                (\t -> Token.setTokens t (Token.countTokens t wounds))
                (investigatorTokens attrs)
                [Token.Damage, Token.Horror]
          , investigatorAssignedHealthDamage = 0
          , investigatorAssignedSanityDamage = 0
          , investigatorCardsUnderneath = filter (`notElem` weaknesses) attrs.cardsUnderneath
          }
    _ -> YourOtherHalf <$> liftRunMessage msg attrs
