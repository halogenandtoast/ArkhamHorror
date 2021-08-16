module Arkham.Types.Investigator.Cards.RolandBanks
  ( RolandBanks(..)
  , rolandBanks
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Stats
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait

newtype RolandBanks = RolandBanks InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

rolandBanks :: RolandBanks
rolandBanks = RolandBanks $ baseAttrs
  "01001"
  ("Roland Banks" <:> "The Fed")
  Guardian
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 3
    , combat = 4
    , agility = 2
    }
  [Agency, Detective]

instance HasAbilities env RolandBanks where
  getAbilities _ _ (RolandBanks a) = pure
    [ restrictedAbility
          a
          1
          (Self <> LocationExists (YourLocation <> LocationWithClues))
          (ReactionAbility (EnemyDefeated Timing.After You AnyEnemy) Free)
        & (abilityLimitL .~ PlayerLimit PerRound 1)
    ]

instance HasCount ClueCount env LocationId => HasTokenValue env RolandBanks where
  getTokenValue (RolandBanks attrs) iid ElderSign | iid == toId attrs = do
    locationClueCount <- unClueCount <$> getCount (investigatorLocation attrs)
    pure $ TokenValue ElderSign (PositiveModifier locationClueCount)
  getTokenValue (RolandBanks attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env RolandBanks where
  runMessage msg rb@(RolandBanks a) = case msg of
    UseCardAbility _ source _ 1 _ | isSource a source -> rb <$ push
      (DiscoverCluesAtLocation (toId a) (investigatorLocation a) 1 Nothing)
    _ -> RolandBanks <$> runMessage msg a
