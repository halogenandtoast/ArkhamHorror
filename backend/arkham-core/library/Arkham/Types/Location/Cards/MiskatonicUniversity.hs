{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MiskatonicUniversity where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype MiskatonicUniversity = MiskatonicUniversity Attrs
  deriving newtype (Show, ToJSON, FromJSON)

miskatonicUniversity :: MiskatonicUniversity
miskatonicUniversity = MiskatonicUniversity $ (baseAttrs
                                                "01129"
                                                "Miskatonic University"
                                                4
                                                (PerPlayer 2)
                                                Diamond
                                                [T, Plus, Circle, Square]
                                                [Arkham]
                                              )
  { locationVictory = Just 1
  }

instance (ActionRunner env investigator) => HasActions env investigator MiskatonicUniversity where
  getActions i NonFast (MiskatonicUniversity attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility (LocationSource "01129") 1 (ActionAbility 1 Nothing))
         | locationRevealed
           && getId () i
           `elem` locationInvestigators
           && hasActionsRemaining i Nothing locationTraits
         ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs@Attrs {..}) = case msg of
    UseCardAbility iid (LocationSource lid) _ 1 | lid == locationId ->
      l <$ unshiftMessage
        (SearchTopOfDeck
          iid
          (InvestigatorTarget iid)
          6
          [Tome, Spell]
          ShuffleBackIn
        )
    _ -> MiskatonicUniversity <$> runMessage msg attrs
