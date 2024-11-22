module Arkham.Event.Events.Decoy (decoy, Decoy (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (PlayCard)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier
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
        $ restricted a 1 InYourHand
        $ ReactionAbility
          (PlayCard #when You (basic $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    , withTooltip
        "{reaction} When you play Decoy, increase its cost by 2: Change \"at your location\" to \"at a location up to 2 connections away.\""
        $ restricted a 2 InYourHand
        $ ForcedWhen (Negate $ EnemyCriteria $ EnemyExists $ EnemyAt YourLocation <> NonEliteEnemy)
        $ ReactionAbility
          (PlayCard #when You (basic $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    ]

instance RunMessage Decoy where
  runMessage msg e@(Decoy attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
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
          <> oneOf
            ( at_ (locationWithInvestigator iid)
                : [ at_ (LocationWithDistanceFrom n (locationWithInvestigator iid) Anywhere) | upToTwoAway, n <- [1 .. 2]
                  ]
            )

      let
        performEvade :: ReverseQueue m => ChooseT m ()
        performEvade = targets enemies $ automaticallyEvadeEnemy iid
      if enemyCount == 2 && length enemies > 1
        then chooseOneM iid do
          labeled "Evade 1 enemy" do
            chooseOrRunOneM iid performEvade
          labeled "Evade 2 enemies" do
            chooseOrRunNM iid 2 performEvade
        else chooseOrRunOneM iid performEvade
      pure e
    InHand _ (UseThisAbility _ (isSource attrs -> True) 1) -> do
      eventModifier attrs (toCardId attrs) $ MetaModifier $ object ["enemyCount" .= (2 :: Int)]
      pure e
    InHand _ (UseThisAbility _ (isSource attrs -> True) 2) -> do
      eventModifier attrs (toCardId attrs) $ MetaModifier $ object ["upToTwoAway" .= True]
      pure e
    _ -> Decoy <$> liftRunMessage msg attrs
