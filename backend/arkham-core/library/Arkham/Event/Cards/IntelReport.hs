module Arkham.Event.Cards.IntelReport
  ( intelReport
  , IntelReport(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message hiding ( PlayCard )
import Arkham.Timing qualified as Timing
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap

newtype IntelReport = IntelReport EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intelReport :: EventCard IntelReport
intelReport = event IntelReport Cards.intelReport

instance HasAbilities IntelReport where
  getAbilities (IntelReport a) =
    [ withTooltip
        "{reaction} When you play Intel Report, increase its cost by 2: Change \"Discover 1 clue\" to \"Discover 2 clues.\""
      $ restrictedAbility a 1 InYourHand
      $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    , withTooltip
        "{reaction} When you play Intel Report, increase its cost by 2: Change \"at your location\" to \"at a location up to 2 connections away.\""
      $ restrictedAbility a 2 InYourHand
      $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    ]

instance RunMessage IntelReport where
  runMessage msg e@(IntelReport attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      modifiers' <- getModifiers (toTarget $ toCardId attrs)

      let
        updateClueCount :: Int -> ModifierType -> Int
        updateClueCount n (MetaModifier (Object o)) =
          case fromJSON <$> KeyMap.lookup "clueCount" o of
            Just (Success a) -> a
            _ -> n
        updateClueCount n _ = n
        clueCount = foldl' updateClueCount 1 modifiers'
        updateDiscoverUpToTwoAway :: Bool -> ModifierType -> Bool
        updateDiscoverUpToTwoAway n (MetaModifier (Object o)) =
          case fromJSON <$> KeyMap.lookup "discoverUpToTwoAway" o of
            Just (Success a) -> a
            _ -> n
        updateDiscoverUpToTwoAway n _ = n
        discoverUpToTwoAway = foldl' updateDiscoverUpToTwoAway False modifiers'

      if discoverUpToTwoAway
        then do
          lids <-
            selectList
            $ LocationMatchAny
            $ (locationWithInvestigator iid <> LocationWithAnyClues)
            : [ LocationWithDistanceFrom n LocationWithAnyClues
              | n <- [1 .. 2]
              ]
          push $ chooseOrRunOne
            iid
            [ targetLabel
                lid
                [InvestigatorDiscoverClues iid lid clueCount Nothing]
            | lid <- lids
            ]
        else push
          $ InvestigatorDiscoverCluesAtTheirLocation iid clueCount Nothing
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 1 _ _) -> do
      push $ CreateWindowModifierEffect
        EffectEventWindow
        (EffectModifiers $ toModifiers
          attrs
          [MetaModifier $ object ["clueCount" .= (2 :: Int)]]
        )
        (toSource attrs)
        (CardIdTarget $ toCardId attrs)
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 2 _ _) -> do
      push $ CreateWindowModifierEffect
        EffectEventWindow
        (EffectModifiers $ toModifiers
          attrs
          [MetaModifier $ object ["discoverUpToTwoAway" .= True]]
        )
        (toSource attrs)
        (CardIdTarget $ toCardId attrs)
      pure e
    _ -> IntelReport <$> runMessage msg attrs
