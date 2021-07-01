module Arkham.Types.Location.Cards.FrozenSpring
  ( frozenSpring
  , FrozenSpring(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (frozenSpring)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype FrozenSpring = FrozenSpring LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenSpring :: LocationId -> FrozenSpring
frozenSpring =
  FrozenSpring
    . (revealedSymbolL .~ Plus)
    . (revealedConnectedSymbolsL .~ setFromList [Triangle, Hourglass])
    . (unrevealedNameL .~ "Diverging Path")
    . baseAttrs
        Cards.frozenSpring
        3
        (PerPlayer 1)
        NoSymbol
        []

instance HasModifiersFor env FrozenSpring where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env FrozenSpring where
  getActions iid (AfterRevealLocation You) (FrozenSpring attrs)
    | iid `on` attrs = do
      pure [ActivateCardAbilityAction iid (forcedAbility attrs)]
  getActions iid window (FrozenSpring attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env FrozenSpring where
  runMessage msg l@(FrozenSpring attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ unshiftMessages
        [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
    _ -> FrozenSpring <$> runMessage msg attrs
