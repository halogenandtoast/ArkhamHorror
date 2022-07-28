module Arkham.Treachery.Cards.DismalCurse
  ( dismalCurse
  , DismalCurse(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DismalCurse = DismalCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dismalCurse :: TreacheryCard DismalCurse
dismalCurse = treachery DismalCurse Cards.dismalCurse

instance HasModifiersFor DismalCurse where
  getModifiersFor (SkillTestSource iid _ source _) (InvestigatorTarget iid') (DismalCurse a)
    | iid == iid' && isSource a source
    = pure $ toModifiers a [Difficulty 2]
  getModifiersFor _ _ _ = pure []

instance RunMessage DismalCurse where
  runMessage msg t@(DismalCurse attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ RevelationSkillTest iid source SkillWillpower 3
      , Discard $ toTarget attrs
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        horror <- field InvestigatorHorror iid
        sanity <- field InvestigatorSanity iid
        let damage = if horror > sanity * 2 then 4 else 2
        push $ InvestigatorAssignDamage iid source DamageAny damage 0
        pure t
    _ -> DismalCurse <$> runMessage msg attrs
