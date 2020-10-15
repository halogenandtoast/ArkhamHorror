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

instance IsInvestigator investigator => HasModifiersFor env investigator FauborgMarigny where
  getModifiersFor _ i (FauborgMarigny attrs) =
    pure [ ReduceCostOfCardType AssetType 1 | atLocation i attrs ]

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Resign))

instance (IsInvestigator investigator) => HasActions env investigator FauborgMarigny where
  getActions i NonFast (FauborgMarigny attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction (getId () i) (ability attrs)
         | atLocation i attrs
           && hasActionsRemaining i (Just Action.Resign) locationTraits
         ]
  getActions i window (FauborgMarigny attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env FauborgMarigny where
  runMessage msg l@(FauborgMarigny attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (Resign iid)
    _ -> FauborgMarigny <$> runMessage msg attrs
