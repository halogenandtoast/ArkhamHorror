{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.FoulSwamp
  ( FoulSwamp(..)
  , foulSwamp
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype FoulSwamp = FoulSwamp Attrs
  deriving newtype (Show, ToJSON, FromJSON)

foulSwamp :: FoulSwamp
foulSwamp = FoulSwamp $ baseAttrs
  "81016"
  "Foul Swamp"
  EncounterSet.CurseOfTheRougarou
  2
  (Static 0)
  Hourglass
  [Equals, Square, Triangle, Diamond]
  [Unhallowed, Bayou]

instance HasModifiersFor env FoulSwamp where
  getModifiersFor _ (InvestigatorTarget iid) (FoulSwamp attrs)
    | iid `member` locationInvestigators attrs = pure
      [CannotHealHorror, CannotCancelHorror]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityMetadata = Just (IntMetadata 0)
  }

instance ActionRunner env => HasActions env FoulSwamp where
  getActions iid NonFast (FoulSwamp attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions iid NonFast attrs
    canAffordActions <- getCanAffordCost
      iid
      (toSource attrs)
      (ActionCost 1 Nothing locationTraits)
    pure
      $ baseActions
      <> [ ActivateCardAbilityActionWithDynamicCost iid (ability attrs)
         | iid `member` locationInvestigators && canAffordActions
         ]
  getActions i window (FoulSwamp attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env FoulSwamp where
  runMessage msg l@(FoulSwamp attrs) = case msg of
    PayForCardAbility iid source meta@(Just (IntMetadata n)) 1
      | isSource attrs source -> if n == 3
        then runMessage (UseCardAbility iid source meta 1) l
        else do
          unshiftMessage $ chooseOne
            iid
            [ Run
              [ InvestigatorAssignDamage iid (toSource attrs) 0 1
              , PayForCardAbility iid source (Just (IntMetadata $ n + 1)) 1
              ]
            , Label
              ("Test with +" <> tshow n <> " Willpower")
              [UseCardAbility iid source meta 1]
            ]
          pure l
    UseCardAbility iid source (Just (IntMetadata n)) 1
      | isSource attrs source -> l <$ unshiftMessages
        [ CreateSkillTestEffect
          (EffectModifiers [SkillModifier SkillWillpower n])
          source
          (InvestigatorTarget iid)
        , BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 7
        ]
    PassedSkillTest _ _ source _ _ | isSource attrs source ->
      l <$ unshiftMessage (Remember FoundAnAncientBindingStone)
    _ -> FoulSwamp <$> runMessage msg attrs
