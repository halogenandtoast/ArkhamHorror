module Arkham.Types.Location.Cards.FoulSwamp
  ( FoulSwamp(..)
  , foulSwamp
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype FoulSwamp = FoulSwamp LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foulSwamp :: FoulSwamp
foulSwamp = FoulSwamp $ baseAttrs
  "81016"
  (Name "Foul Swamp" Nothing)
  EncounterSet.CurseOfTheRougarou
  2
  (Static 0)
  Hourglass
  [Equals, Square, Triangle, Diamond]
  [Unhallowed, Bayou]

instance HasModifiersFor env FoulSwamp where
  getModifiersFor _ (InvestigatorTarget iid) (FoulSwamp attrs)
    | iid `member` locationInvestigators attrs = pure
    $ toModifiers attrs [CannotHealHorror, CannotCancelHorror]
  getModifiersFor _ _ _ = pure []

ability :: InvestigatorId -> LocationAttrs -> Ability
ability iid attrs = base { abilityMetadata = Just (IntMetadata 0) }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ActionAbility Nothing $ Costs
      [ ActionCost 1
      , UpTo 3 (HorrorCost (toSource attrs) (InvestigatorTarget iid) 1)
      ]
    )

instance ActionRunner env => HasActions env FoulSwamp where
  getActions iid NonFast (FoulSwamp attrs@LocationAttrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability iid attrs)
      | iid `member` locationInvestigators
      ]
  getActions i window (FoulSwamp attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env FoulSwamp where
  runMessage msg l@(FoulSwamp attrs) = case msg of
    PayForCardAbility iid source meta@(Just (IntMetadata n)) 1
      | isSource attrs source -> if n == 3
        then runMessage (UseCardAbility iid source meta 1 NoPayment) l
        else do
          unshiftMessage $ chooseOne
            iid
            [ Run
              [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
              , PayForCardAbility iid source (Just (IntMetadata $ n + 1)) 1
              ]
            , Label
              ("Test with +" <> tshow n <> " Willpower")
              [UseCardAbility iid source meta 1 NoPayment]
            ]
          pure l
    UseCardAbility iid source (Just (IntMetadata n)) 1 _
      | isSource attrs source -> l <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillWillpower n])
          source
          (InvestigatorTarget iid)
        , BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 7
        ]
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      l <$ unshiftMessage (Remember FoundAnAncientBindingStone)
    _ -> FoulSwamp <$> runMessage msg attrs
