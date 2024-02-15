module Arkham.Event.Cards.Decoy (
  decoy,
  Decoy (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Timing qualified as Timing
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap

newtype Decoy = Decoy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decoy :: EventCard Decoy
decoy = event Decoy Cards.decoy

instance HasAbilities Decoy where
  getAbilities (Decoy a) =
    [ withTooltip
        "{reaction}  When you play Decoy, increase its cost by 2: Change \"a non-Elite enemy\" to \"up to 2 non-Elite enemies.\""
        $ restrictedAbility a 1 InYourHand
        $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    , withTooltip
        "{reaction} When you play Decoy, increase its cost by 2: Change \"at your location\" to \"at a location up to 2 connections away.\""
        $ restrictedAbility a 2 InYourHand
        $ ForcedWhen (Negate $ EnemyCriteria $ EnemyExists $ EnemyAt YourLocation <> NonEliteEnemy)
        $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    ]

instance RunMessage Decoy where
  runMessage msg e@(Decoy attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      modifiers' <- getModifiers (toTarget $ toCardId attrs)

      let
        updateEnemyCount :: Int -> ModifierType -> Int
        updateEnemyCount n (MetaModifier (Object o)) =
          case fromJSON <$> KeyMap.lookup "enemyCount" o of
            Just (Success a) -> a
            _ -> n
        updateEnemyCount n _ = n
        enemyCount = foldl' updateEnemyCount 1 modifiers'
        updateUpToTwoAway :: Bool -> ModifierType -> Bool
        updateUpToTwoAway n (MetaModifier (Object o)) =
          case fromJSON <$> KeyMap.lookup "upToTwoAway" o of
            Just (Success a) -> a
            _ -> n
        updateUpToTwoAway n _ = n
        upToTwoAway = foldl' updateUpToTwoAway False modifiers'

      enemies <-
        select
          $ NonEliteEnemy
          <> EnemyOneOf
            ( EnemyAt (locationWithInvestigator iid)
                : [ EnemyAt (LocationWithDistanceFrom n Anywhere)
                  | upToTwoAway
                  , n <- [1 .. 2]
                  ]
            )

      player <- getPlayer iid
      let
        targets = [targetLabel enemy [EnemyEvaded iid enemy] | enemy <- enemies]
        evadeOneEnemy = chooseOrRunOne player targets

      push
        $ if enemyCount == 2 && length targets > 1
          then
            chooseOne
              player
              [Label "Evade 1 enemy" [evadeOneEnemy], Label "Evade 2 enemies" [chooseOrRunN player 2 targets]]
          else evadeOneEnemy
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 1 _ _) -> do
      push
        $ CreateWindowModifierEffect
          EffectEventWindow
          ( EffectModifiers
              $ toModifiers
                attrs
                [MetaModifier $ object ["enemyCount" .= (2 :: Int)]]
          )
          (toSource attrs)
          (CardIdTarget $ toCardId attrs)
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 2 _ _) -> do
      push
        $ CreateWindowModifierEffect
          EffectEventWindow
          ( EffectModifiers
              $ toModifiers
                attrs
                [MetaModifier $ object ["upToTwoAway" .= True]]
          )
          (toSource attrs)
          (CardIdTarget $ toCardId attrs)
      pure e
    _ -> Decoy <$> runMessage msg attrs
