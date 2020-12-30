{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.VipArea
  ( vipArea
  , VipArea(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Phase
import Arkham.Types.Trait hiding (Cultist)

newtype VipArea = VipArea Attrs
  deriving newtype (Show, ToJSON, FromJSON)

vipArea :: VipArea
vipArea = VipArea $ (baseAttrs
                      "02076"
                      (LocationName "VIP Area" Nothing)
                      EncounterSet.TheHouseAlwaysWins
                      3
                      (PerPlayer 1)
                      T
                      [Diamond]
                      [CloverClub]
                    )
  { locationVictory = Just 1
  , locationRevealedSymbol = Plus
  }

instance HasPhase env => HasModifiersFor env VipArea where
  getModifiersFor _ (InvestigatorTarget iid) (VipArea attrs)
    | iid `member` locationInvestigators attrs = do
      phase <- getPhase <$> ask
      if phase == UpkeepPhase
        then pure $ toModifiers attrs [CannotDrawCards, CannotGainResources]
        else pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env VipArea where
  getActions iid window (VipArea attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env VipArea where
  runMessage msg (VipArea attrs@Attrs {..}) = VipArea <$> runMessage msg attrs
