{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.TrappersCabin where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype TrappersCabin = TrappersCabin Attrs
  deriving newtype (Show, ToJSON, FromJSON)

trappersCabin :: TrappersCabin
trappersCabin = TrappersCabin $ baseAttrs
  "81014"
  "Trapper's Cabin"
  3
  (Static 0)
  Moon
  [Diamond, Moon]
  [Wilderness]

instance IsInvestigator investigator => HasModifiersFor env investigator TrappersCabin where
  getModifiersFor _ i (TrappersCabin Attrs { locationInvestigators }) = pure
    [ CannotGainResources
    | getId () i `member` locationInvestigators
    ]

instance (IsInvestigator investigator) => HasActions env investigator TrappersCabin where
  getActions i NonFast (TrappersCabin attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility
               (toSource attrs)
               1
               (ActionAbility 1 Nothing)
             )
         | getId () i `elem` locationInvestigators && resourceCount i >= 5
         ]
  getActions i window (TrappersCabin attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TrappersCabin where
  runMessage msg l@(TrappersCabin attrs) =  case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage
        (BeginSkillTest
          iid
          source
          (toTarget attrs)
          Nothing
          SkillIntellect
          3
          [TakeControlOfSetAsideAsset iid "81029"]
          mempty
          mempty
          mempty)
    _ -> TrappersCabin <$> runMessage msg attrs
