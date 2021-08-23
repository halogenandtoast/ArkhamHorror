module Arkham.Types.Location.Cards.FoulSwamp
  ( FoulSwamp(..)
  , foulSwamp
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (foulSwamp)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype FoulSwamp = FoulSwamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foulSwamp :: LocationCard FoulSwamp
foulSwamp = location
  FoulSwamp
  Cards.foulSwamp
  2
  (Static 0)
  Hourglass
  [Equals, Square, Triangle, Diamond]

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

instance HasAbilities env FoulSwamp where
  getAbilities iid window@(Window Timing.When NonFast) (FoulSwamp attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseAbilities iid window attrs
      $ pure [locationAbility (ability iid attrs)]
  getAbilities i window (FoulSwamp attrs) = getAbilities i window attrs

instance LocationRunner env => RunMessage env FoulSwamp where
  runMessage msg l@(FoulSwamp attrs) = case msg of
    PayForCardAbility iid source windows 1 payment@(HorrorPayment n)
      | isSource attrs source -> if n == 3
        then runMessage (UseCardAbility iid source windows 1 payment) l
        else do
          push $ chooseOne
            iid
            [ Run
              [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
              , PayForCardAbility iid source windows 1 (HorrorPayment $ n + 1)
              ]
            , Label
              ("Test with +" <> tshow n <> " Willpower")
              [UseCardAbility iid source windows 1 payment]
            ]
          pure l
    UseCardAbility iid source _ 1 (HorrorPayment n) | isSource attrs source ->
      l <$ pushAll
        [ skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower n)
        , BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 7
        ]
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember FoundAnAncientBindingStone)
    _ -> FoulSwamp <$> runMessage msg attrs
