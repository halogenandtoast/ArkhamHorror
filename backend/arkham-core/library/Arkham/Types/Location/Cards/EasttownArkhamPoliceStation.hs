{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.EasttownArkhamPoliceStation
  ( EasttownArkhamPoliceStation(..)
  , easttownArkhamPoliceStation
  )
where

import Arkham.Import

import Arkham.Types.Asset.Uses
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype EasttownArkhamPoliceStation = EasttownArkhamPoliceStation Attrs
  deriving newtype (Show, ToJSON, FromJSON)

easttownArkhamPoliceStation :: EasttownArkhamPoliceStation
easttownArkhamPoliceStation =
  EasttownArkhamPoliceStation $ (baseAttrs
                                  "50027"
                                  "Easttown"
                                  EncounterSet.ReturnToTheMidnightMasks
                                  4
                                  (PerPlayer 2)
                                  Moon
                                  [Circle, Triangle]
                                  [Arkham]
                                )
    { locationVictory = Just 1
    }

instance HasModifiersFor env EasttownArkhamPoliceStation where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env EasttownArkhamPoliceStation where
  getActions iid NonFast (EasttownArkhamPoliceStation attrs)
    | locationRevealed attrs = do
      baseActions <- getActions iid NonFast attrs
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList $ locationTraits attrs)
      unused <- getIsUnused iid (ability attrs)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | unused
             && iid
             `member` locationInvestigators attrs
             && hasActionsRemaining
           ]
  getActions iid window (EasttownArkhamPoliceStation attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env EasttownArkhamPoliceStation where
  runMessage msg l@(EasttownArkhamPoliceStation attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      ammoAssets <- map (Ammo, ) <$> getSetList (iid, Ammo)
      resourceAssets <- map (Resource, ) <$> getSetList (iid, Resource)
      l <$ unshiftMessage
        (chooseOne
          iid
          [ AddUses (AssetTarget assetId) useType' 2
          | (useType', assetId) <- ammoAssets <> resourceAssets
          ]
        )
    _ -> EasttownArkhamPoliceStation <$> runMessage msg attrs
