module Arkham.Asset.Cards.NineOfRods3 (
  nineOfRods3,
  NineOfRods3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Window (Window, defaultWindows, windowType)
import Arkham.Window qualified as Window

newtype NineOfRods3 = NineOfRods3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nineOfRods3 :: AssetCard NineOfRods3
nineOfRods3 = asset NineOfRods3 Cards.nineOfRods3

instance HasAbilities NineOfRods3 where
  getAbilities (NineOfRods3 a) =
    [ reactionAbility
        a
        1
        (exhaust a)
        (DrawCard #when You (CanCancelRevelationEffect $ BasicCardMatch NonWeaknessTreachery) EncounterDeck)
        ControlsThis
    , restrictedAbility a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

getCard :: [Window] -> Card
getCard = \case
  [] -> error "impossible"
  ((windowType -> Window.DrawCard _ card _) : _) -> card
  (_ : rest) -> getCard rest

instance RunMessage NineOfRods3 where
  runMessage msg a@(NineOfRods3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCard -> card) _ -> do
      target <- case toCardType card of
        EnemyType -> toTarget <$> selectJust (EnemyWithCardId $ toCardId card)
        TreacheryType -> toTarget <$> selectJust (TreacheryWithCardId $ toCardId card)
        LocationType -> toTarget <$> selectJust (LocationWithCardId $ toCardId card)
        AssetType -> toTarget <$> selectJust (AssetWithCardId $ toCardId card)
        _ -> error "Unhandled type"
      pushAll
        [ CancelNext (toSource attrs) RevelationMessage
        , CancelNext (toSource attrs) DrawEnemyMessage
        , CancelSurge (toSource attrs)
        , ShuffleIntoDeck Deck.EncounterDeck target
        , InvestigatorDrawEncounterCard iid
        ]
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> NineOfRods3 <$> runMessage msg attrs
