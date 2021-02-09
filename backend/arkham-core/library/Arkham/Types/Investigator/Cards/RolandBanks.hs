module Arkham.Types.Investigator.Cards.RolandBanks
  ( RolandBanks(..)
  , rolandBanks
  )
where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype RolandBanks = RolandBanks InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env RolandBanks where
  getModifiersFor source target (RolandBanks attrs) =
    getModifiersFor source target attrs

rolandBanks :: RolandBanks
rolandBanks = RolandBanks
  $ baseAttrs "01001" "Roland Banks" Guardian stats [Agency, Detective]
 where
  stats = Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 3
    , combat = 4
    , agility = 2
    }

ability :: InvestigatorAttrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerRound 1 }
  where base = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance ActionRunner env => HasActions env RolandBanks where
  getActions iid (WhenEnemyDefeated You) (RolandBanks a) | iid == toId a = do
    clueCount <- unClueCount <$> getCount (investigatorLocation a)
    pure [ ActivateCardAbilityAction iid (ability a) | clueCount > 0 ]
  getActions _ _ _ = pure []

instance HasCount ClueCount env LocationId => HasTokenValue env RolandBanks where
  getTokenValue (RolandBanks attrs) iid ElderSign | iid == toId attrs = do
    locationClueCount <- unClueCount <$> getCount (investigatorLocation attrs)
    pure $ TokenValue ElderSign (PositiveModifier locationClueCount)
  getTokenValue (RolandBanks attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env RolandBanks where
  runMessage msg rb@(RolandBanks attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      rb <$ unshiftMessage
        (DiscoverCluesAtLocation (toId attrs) investigatorLocation 1 Nothing)
    _ -> RolandBanks <$> runMessage msg attrs
