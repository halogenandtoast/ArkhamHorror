{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.FauborgMarigny
  ( FauborgMarigny(..)
  , fauborgMarigny
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype FauborgMarigny = FauborgMarigny Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fauborgMarigny :: FauborgMarigny
fauborgMarigny = FauborgMarigny $ baseAttrs
  "81012"
  "Faurborg Marigny"
  4
  (Static 0)
  Squiggle
  [Triangle, Squiggle]
  [Riverside]

instance HasModifiersFor env FauborgMarigny where
  getModifiersFor _ (InvestigatorTarget iid) (FauborgMarigny attrs) = pure
    [ ReduceCostOfCardType AssetType 1
    | iid `member` locationInvestigators attrs
    ]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Resign))

instance ActionRunner env => HasActions env FauborgMarigny where
  getActions iid NonFast (FauborgMarigny attrs@Attrs {..}) | locationRevealed =
    do
      baseActions <- getActions iid NonFast attrs
      hasActionsRemaining <- getHasActionsRemaining
        iid
        (Just Action.Resign)
        (setToList locationTraits)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | iid `member` locationInvestigators && hasActionsRemaining
           ]
  getActions i window (FauborgMarigny attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env FauborgMarigny where
  runMessage msg l@(FauborgMarigny attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (Resign iid)
    _ -> FauborgMarigny <$> runMessage msg attrs
