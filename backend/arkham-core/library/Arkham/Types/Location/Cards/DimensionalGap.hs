module Arkham.Types.Location.Cards.DimensionalGap
  ( dimensionalGap
  , DimensionalGap(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Trait
import Arkham.Types.Window

newtype DimensionalGap = DimensionalGap LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalGap :: DimensionalGap
dimensionalGap =
  DimensionalGap
    $ baseAttrs
        "02289"
        (Name "Dimensional Gap" Nothing)
        EncounterSet.WhereDoomAwaits
        3
        (PerPlayer 1)
        NoSymbol
        []
        [Dunwich, Woods, Altered]
    & (revealedSymbolL .~ T)
    & (revealedConnectedSymbolsL .~ setFromList [Square, Moon])
    & (unrevealedNameL .~ LocationName (mkName "Altered Path"))

instance HasModifiersFor env DimensionalGap where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env DimensionalGap where
  getActions iid (AfterRevealLocation You) (DimensionalGap attrs)
    | iid `on` attrs = pure
      [ActivateCardAbilityAction iid (forcedAbility attrs)]
  getActions iid window (DimensionalGap attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DimensionalGap where
  runMessage msg l@(DimensionalGap attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      l <$ unshiftMessage
        (DiscardEncounterUntilFirst source
        $ EncounterCardMatchByType (EnemyType, Nothing)
        )
    RequestedEncounterCard source (Just ec) | isSource attrs source ->
      l <$ unshiftMessage (SpawnEnemyAt (EncounterCard ec) (toId attrs))
    _ -> DimensionalGap <$> runMessage msg attrs
