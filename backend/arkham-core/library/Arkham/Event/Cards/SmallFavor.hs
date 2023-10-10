module Arkham.Event.Cards.SmallFavor (
  smallFavor,
  SmallFavor (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Timing qualified as Timing
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap

newtype SmallFavor = SmallFavor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smallFavor :: EventCard SmallFavor
smallFavor = event SmallFavor Cards.smallFavor

instance HasAbilities SmallFavor where
  getAbilities (SmallFavor a) =
    [ withTooltip
        "{reaction} When you play Small Favor, increase its cost by 2: Change \"Deal 1 damage\" to \"Deal 2 damage.\""
        $ restrictedAbility a 1 InYourHand
        $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    , withTooltip
        "{reaction} When you play Small Favor, increase its cost by 2: Change \"at your location\" to \"at a location up to 2 connections away.\""
        $ restrictedAbility a 2 InYourHand
        $ ForcedWhen
          ( Negate
              $ EnemyCriteria
              $ EnemyExists
              $ EnemyAt YourLocation
              <> NonEliteEnemy
          )
        $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    ]

instance RunMessage SmallFavor where
  runMessage msg e@(SmallFavor attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      modifiers' <- getModifiers (toTarget $ toCardId attrs)

      let
        updateDamageCount :: Int -> ModifierType -> Int
        updateDamageCount n (MetaModifier (Object o)) =
          case fromJSON <$> KeyMap.lookup "damageCount" o of
            Just (Success a) -> a
            _ -> n
        updateDamageCount n _ = n
        damageCount = foldl' updateDamageCount 1 modifiers'
        updateUpToTwoAway :: Bool -> ModifierType -> Bool
        updateUpToTwoAway n (MetaModifier (Object o)) =
          case fromJSON <$> KeyMap.lookup "upToTwoAway" o of
            Just (Success a) -> a
            _ -> n
        updateUpToTwoAway n _ = n
        upToTwoAway = foldl' updateUpToTwoAway False modifiers'

      enemies <-
        selectList
          $ NonEliteEnemy
          <> EnemyOneOf
            ( EnemyAt (locationWithInvestigator iid)
                : [ EnemyAt (LocationWithDistanceFrom n Anywhere)
                  | upToTwoAway
                  , n <- [1 .. 2]
                  ]
            )

      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel enemy [EnemyDamage enemy $ nonAttack attrs damageCount]
          | enemy <- enemies
          ]
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 1 _ _) -> do
      push
        $ CreateWindowModifierEffect
          EffectEventWindow
          ( EffectModifiers
              $ toModifiers
                attrs
                [MetaModifier $ object ["damageCount" .= (2 :: Int)]]
          )
          (toSource attrs)
          (CardIdTarget $ toCardId attrs)
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 2 _ _) -> do
      push
        $ CreateWindowModifierEffect
          EffectEventWindow
          ( EffectModifiers
              $ toModifiers attrs [MetaModifier $ object ["upToTwoAway" .= True]]
          )
          (toSource attrs)
          (CardIdTarget $ toCardId attrs)
      pure e
    _ -> SmallFavor <$> runMessage msg attrs
