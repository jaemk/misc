package d04

import org.junit.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class D04Test {
    @Test
    fun testValidPass() {
        assertTrue { validPass(111111) }
        assertFalse { validPass(223450) }
        assertFalse { validPass(123789) }
    }

    @Test
    fun testValidPass2() {
        assertTrue { validPass2(112233) }
        assertFalse { validPass2(123444) }
        assertTrue { validPass2(111122) }
    }
}
