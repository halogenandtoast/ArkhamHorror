module Arkham.Treachery.Cards.GlimpseOfTheUnderworld (glimpseOfTheUnderworld, GlimpseOfTheUnderworld (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Damage
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GlimpseOfTheUnderworld = GlimpseOfTheUnderworld TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseOfTheUnderworld :: TreacheryCard GlimpseOfTheUnderworld
glimpseOfTheUnderworld = treachery GlimpseOfTheUnderworld Cards.glimpseOfTheUnderworld

instance HasAbilities GlimpseOfTheUnderworld where
  getAbilities (GlimpseOfTheUnderworld attrs) =
    [ restrictedAbility attrs 1 (InThreatAreaOf You)
        $ forced
        $ oneOf
          [ InvestigatorWouldTakeDamage #when You AnySource
          , InvestigatorWouldTakeHorror #when You AnySource
          ]
    , fastAbility attrs 2 Free (InThreatAreaOf You)
    ]

toDamageType :: [Window] -> DamageType
toDamageType [] = error "invalid window"
toDamageType ((windowType -> wType) : rest) = case wType of
  Window.WouldTakeDamage {} -> DamageType
  Window.WouldTakeHorror {} -> HorrorType
  _ -> toDamageType rest

instance RunMessage GlimpseOfTheUnderworld where
  runMessage msg t@(GlimpseOfTheUnderworld attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (toDamageType -> damageType) _ -> do
      case damageType of
        DamageType -> dealAdditionalDamage iid 1 []
        HorrorType -> dealAdditionalHorror iid 1 []
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      pushAll [toDiscardBy iid (attrs.ability 2) attrs, assignDamageAndHorror iid (attrs.ability 2) 1 1]
      pure t
    _ -> GlimpseOfTheUnderworld <$> runMessage msg attrs
