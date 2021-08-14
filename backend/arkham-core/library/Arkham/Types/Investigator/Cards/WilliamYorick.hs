module Arkham.Types.Investigator.Cards.WilliamYorick where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window
import System.IO.Unsafe

newtype WilliamYorick = WilliamYorick InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env WilliamYorick where
  getModifiersFor source target (WilliamYorick attrs) =
    getModifiersFor source target attrs

williamYorick :: WilliamYorick
williamYorick = WilliamYorick $ baseAttrs
  "03005"
  "William Yorick"
  Survivor
  Stats
    { health = 8
    , sanity = 6
    , willpower = 3
    , intellect = 2
    , combat = 4
    , agility = 3
    }
  [Warden]

instance HasTokenValue env WilliamYorick where
  getTokenValue (WilliamYorick attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (WilliamYorick attrs) iid token = getTokenValue attrs iid token

-- because we are checking if cards are playable will we inevitably
-- trigger a case where we need to check for actions. For example
-- "I'm outta here" needs to check for a resign ability. Doing this
-- will cause infinite recursion, so we take out a global lock to
-- ensure we only run this code once in an iteration
williamYorickRecursionLock :: IORef Bool
williamYorickRecursionLock = unsafePerformIO $ newIORef False
{-# NOINLINE williamYorickRecursionLock #-}

instance InvestigatorRunner env => HasAbilities env WilliamYorick where
  getAbilities i (AfterEnemyDefeated who _) (WilliamYorick attrs) | i == who = do
    locked <- readIORef williamYorickRecursionLock
    if locked
      then pure []
      else do
        writeIORef williamYorickRecursionLock True
        let
          targets =
            filter ((== AssetType) . toCardType) (investigatorDiscard attrs)
        playableTargets <- filterM
          (getIsPlayable i [NonFast, DuringTurn i] . PlayerCard)
          targets
        writeIORef williamYorickRecursionLock False
        pure
          [ mkAbility attrs 1 $ ReactionAbility Free | notNull playableTargets ]
  getAbilities i window (WilliamYorick attrs) = getAbilities i window attrs


instance (InvestigatorRunner env) => RunMessage env WilliamYorick where
  runMessage msg i@(WilliamYorick attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      let
        targets =
          filter ((== AssetType) . toCardType) (investigatorDiscard attrs)
        playCardMsgs c =
          [ AddToHand iid c
          , if isDynamic c
            then InitiatePlayDynamicCard iid (toCardId c) 0 Nothing False
            else InitiatePlayCard iid (toCardId c) Nothing False
          ]
      playableTargets <- filterM
        (getIsPlayable iid [NonFast, DuringTurn iid] . PlayerCard)
        targets
      i <$ push
        (chooseOne iid
        $ [ TargetLabel
              (CardIdTarget $ toCardId card)
              (playCardMsgs $ PlayerCard card)
          | card <- playableTargets
          ]
        )
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      i <$ push
        (CreateEffect
          (unInvestigatorId $ toId attrs)
          Nothing
          (TokenEffectSource ElderSign)
          (InvestigatorTarget iid)
        )
    _ -> WilliamYorick <$> runMessage msg attrs
