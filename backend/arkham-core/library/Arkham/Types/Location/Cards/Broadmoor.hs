{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Broadmoor where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Broadmoor = Broadmoor Attrs
  deriving newtype (Show, ToJSON, FromJSON)

broadmoor :: Broadmoor
broadmoor =
  Broadmoor
    $ (baseAttrs
        "81009"
        "Broadmoor"
        3
        (PerPlayer 1)
        Plus
        [Square, Plus]
        [NewOrleans]
      )
        { locationVictory = Just 1
        }

instance HasModifiersFor env investigator Broadmoor where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator Broadmoor where
  getActions i NonFast (Broadmoor attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility
               (toSource attrs)
               1
               (ActionAbility 1 (Just Action.Resign))
             )
         | getId () i `elem` locationInvestigators
         ]
  getActions i window (Broadmoor attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Broadmoor where
  runMessage msg l@(Broadmoor attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (Resign iid)
    _ -> Broadmoor <$> runMessage msg attrs
