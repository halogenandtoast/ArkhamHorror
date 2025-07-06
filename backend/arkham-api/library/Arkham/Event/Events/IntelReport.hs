module Arkham.Event.Events.IntelReport (intelReport) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (PlayCard)
import Arkham.Matcher
import Arkham.Modifier
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
        $ restricted a 1 InYourHand
        $ triggered (PlayCard #when You (basic $ CardWithId a.cardId)) (IncreaseCostOfThis a.cardId 2)
    , withTooltip
        "{reaction} When you play Intel Report, increase its cost by 2: Change \"at your location\" to \"at a location up to 2 connections away.\""
        $ restricted a 2 InYourHand
        $ ForcedWhen (exists $ LocationWithoutClues <> YourLocation)
        $ triggered (PlayCard #when You (basic $ CardWithId a.cardId)) (IncreaseCostOfThis a.cardId 2)
    ]

instance RunMessage IntelReport where
  runMessage msg e@(IntelReport attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      modifiers' <- getModifiers (toCardId attrs)

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
            select
              $ oneOf
              $ (locationWithInvestigator iid <> LocationWithAnyClues)
              : [ LocationWithDistanceFrom n (locationWithInvestigator iid) LocationWithAnyClues
                | n <- [1 .. 2]
                ]
          chooseOrRunOneM iid $ targets lids $ discoverAt NotInvestigate iid attrs clueCount
        else discoverAtYourLocation NotInvestigate iid attrs clueCount
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 1 _ _) -> do
      eventModifier attrs (toCardId attrs) $ MetaModifier $ object ["clueCount" .= (2 :: Int)]
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 2 _ _) -> do
      eventModifier attrs (toCardId attrs) $ MetaModifier $ object ["discoverUpToTwoAway" .= True]
      pure e
    _ -> IntelReport <$> liftRunMessage msg attrs
