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
  (mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Resign)))
    { abilityLimit = PerGame
    }

instance ActionRunner env => HasActions env CloverClubLounge where
  getActions iid NonFast (CloverClubLounge attrs@Attrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      step <- unActStep . getStep <$> ask
      canAffordActions <- getCanAffordCost
        iid
        (toSource attrs)
        (ActionCost 1 Nothing locationTraits)
      canAffordDiscard <- getCanAffordCost
        iid
        (toSource attrs)
        (DiscardCost 1 (Just AssetType) (singleton Ally))
      unused <- getIsUnused iid (ability attrs)
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | iid
          `member` locationInvestigators
          && canAffordActions
          && canAffordDiscard
          && step
          == 1
          && unused
        ]
  getActions iid window (CloverClubLounge attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env CloverClubLounge where
  runMessage msg l@(CloverClubLounge attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source && locationRevealed ->
      do
        cards <-
          filter
            (and
            . sequence [(== AssetType) . pcCardType, member Ally . pcTraits]
            )
          . mapMaybe (preview _PlayerCard)
          <$> getHandOf iid
        l <$ unshiftMessages
          [ chooseOne iid [ DiscardCard iid (getCardId card) | card <- cards ]
          , GainClues iid 2
          ]
    _ -> CloverClubLounge <$> runMessage msg attrs
