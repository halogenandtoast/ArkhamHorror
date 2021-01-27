module Arkham.Types.Location.Cards.CloverClubLounge
  ( cloverClubLounge
  , CloverClubLounge(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype CloverClubLounge = CloverClubLounge Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cloverClubLounge :: CloverClubLounge
cloverClubLounge = CloverClubLounge $ baseAttrs
  "02071"
  (Name "Clover Club Lounge" Nothing)
  EncounterSet.TheHouseAlwaysWins
  2
  (Static 0)
  Circle
  [Moon, Square, Triangle]
  [CloverClub]

instance HasModifiersFor env CloverClubLounge where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs =
  (mkAbility
      (toSource attrs)
      1
      (ActionAbility Nothing
      $ Costs
          [ ActionCost 1
          , HandDiscardCost 1 (Just AssetType) (singleton Ally) mempty
          ]
      )
    )
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasActions env CloverClubLounge where
  getActions iid NonFast (CloverClubLounge attrs@Attrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      step <- unActStep . getStep <$> ask
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | iid `member` locationInvestigators && step == 1
        ]
  getActions iid window (CloverClubLounge attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env CloverClubLounge where
  runMessage msg l@(CloverClubLounge attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed -> l
      <$ unshiftMessage (GainClues iid 2)
    _ -> CloverClubLounge <$> runMessage msg attrs
