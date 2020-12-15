{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.CloverClubLounge
  ( cloverClubLounge
  , CloverClubLounge(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
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
  "Clover Club Lounge"
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
  mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Resign))

instance ActionRunner env => HasActions env CloverClubLounge where
  getActions iid NonFast (CloverClubLounge attrs@Attrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      canAffordActions <- getCanAffordCost
        iid
        (toSource attrs)
        (ActionCost 1 (Just Action.Resign) locationTraits)
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | iid `member` locationInvestigators && canAffordActions
        ]
  getActions iid window (CloverClubLounge attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env CloverClubLounge where
  runMessage msg (CloverClubLounge attrs) =
    CloverClubLounge <$> runMessage msg attrs
