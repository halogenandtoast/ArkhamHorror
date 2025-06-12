module Arkham.Event.Events.SmallFavor (smallFavor) where

import Arkham.Ability
import Arkham.Card
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (PlayCard)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier
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
        $ restricted a 1 InYourHand
        $ triggered
          (PlayCard #when You (basic $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    , withTooltip
        "{reaction} When you play Small Favor, increase its cost by 2: Change \"at your location\" to \"at a location up to 2 connections away.\""
        $ restricted a 2 InYourHand
        $ ForcedWhen (not_ $ exists $ EnemyAt YourLocation <> NonEliteEnemy)
        $ triggered
          (PlayCard #when You (basic $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    ]

instance RunMessage SmallFavor where
  runMessage msg e@(SmallFavor attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
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
        select
          $ NonEliteEnemy
          <> EnemyOneOf
            ( EnemyAt (locationWithInvestigator iid)
                : [ EnemyAt (LocationWithDistanceFrom n (locationWithInvestigator iid) Anywhere)
                  | upToTwoAway
                  , n <- [1 .. 2]
                  ]
            )

      chooseOrRunOneM iid do
        targets enemies \enemy -> push $ EnemyDamage enemy $ nonAttack (Just iid) attrs damageCount
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 1 _ _) -> do
      eventModifier attrs (toCardId attrs) $ MetaModifier $ object ["damageCount" .= (2 :: Int)]
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 2 _ _) -> do
      eventModifier attrs (toCardId attrs) $ MetaModifier $ object ["upToTwoAway" .= True]
      pure e
    _ -> SmallFavor <$> liftRunMessage msg attrs
