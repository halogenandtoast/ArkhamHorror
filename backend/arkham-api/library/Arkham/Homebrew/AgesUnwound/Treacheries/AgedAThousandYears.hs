module Arkham.Homebrew.AgesUnwound.Treacheries.AgedAThousandYears (agedAThousandYears) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers (ModifierType (BaseSkillOf), inThreatAreaGets)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Investigator.Types (
  Field (InvestigatorBaseAgility, InvestigatorBaseCombat),
 )
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype AgedAThousandYears = AgedAThousandYears TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving anyclass IsTreachery

{- | /Aged a Thousand Years/, the reverse of the @:ages-unwound:198@ printing of
/What Could Be/.
-}
agedAThousandYears :: TreacheryCard AgedAThousandYears
agedAThousandYears = treachery AgedAThousandYears Cards.agedAThousandYears

{- | "Set the base value of your [combat] and [agility] to 1."

'BaseSkillOf' is a /set/, not a modifier on the printed value, which is what
"set the base value" asks for. It is hung on the host investigator, so it travels
with the card rather than with the location it was drawn at.
-}
instance HasModifiersFor AgedAThousandYears where
  getModifiersFor (AgedAThousandYears a) =
    inThreatAreaGets a [BaseSkillOf #combat 1, BaseSkillOf #agility 1]

{- | "__Forced__ - At the start of the investigator phase: Discard cards from the
top of the encounter deck until an enemy is discarded. Spawn that enemy engaged
with you. /
[action]: Place 1 doom on the current agenda. Remove Aged a Thousand Years from
the game."
-}
instance HasAbilities AgedAThousandYears where
  getAbilities (AgedAThousandYears a) =
    [ restricted a 1 InYourThreatArea $ forced $ PhaseBegins #when #investigation
    , restricted a 2 InYourThreatArea actionAbility
    ]

{- | "__Revelation__ - The investigator with the highest total base [combat] and
[agility] puts this card into play in their threat area."
-}
instance RunMessage AgedAThousandYears where
  runMessage msg t@(AgedAThousandYears attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      investigators <- select UneliminatedInvestigator
      totals <- for investigators \iid -> do
        combat <- field InvestigatorBaseCombat iid
        agility <- field InvestigatorBaseAgility iid
        pure (combat + agility, iid)
      case sortOn (\(total, _) -> negate total) totals of
        (_, iid) : _ -> placeInThreatArea attrs iid
        [] -> pure ()
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardUntilFirst iid (attrs.ability 1) Deck.EncounterDeck (basic #enemy)
      pure t
    {- "Spawn that enemy engaged with you." The enemy is engaged with the host
    investigator, who is the only prey this card ever names. -}
    RequestedEncounterCard (isAbilitySource attrs 1 -> True) (Just _) (Just card) -> do
      createEnemyEngagedWithPrey_ (toCard card)
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeDoomOnAgenda 1
      removeFromGame attrs
      pure t
    _ -> AgedAThousandYears <$> liftRunMessage msg attrs
